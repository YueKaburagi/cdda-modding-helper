
package cddamod

import scala.language.postfixOps
import scala.language.higherKinds
import scalaz._
import scalaz.Scalaz._

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._
import org.json4s.native.Printer.{pretty}

import java.io.{File, FilenameFilter, FileWriter}

trait Error
case class KeyNotFound(key: String) extends Error {
  override def toString = "KeyNotFound[\""+key+"\"]"
}
object UnexpectedValueType extends Error {
  override def toString = "UnexpectedValueType"
}

trait UT {
  def mapLeft[A,B,C](f: A => C)(t: (A,B)): (C,B) = fFirst(f)(t)
  def fFirst[A,B,C](f: A => C)(t: (A,B)): (C,B) = t match {
    case (a,b) => (f (a), b)
  }
  def mapRight[A,B,C](f: B => C)(t: (A,B)): (A,C) = fSecond(f)(t)
  def fSecond[A,B,C](f: B => C)(t: (A,B)): (A,C) = t match {
    case (a,b) => (a, f (b))
  }
  def fSecondM[A,B,C,F[_]: Functor](f: B => F[C])(t: (A,B)): F[(A,C)] = t match {
    case (a,b) => f(b) map {(a,_)}
  }
  def fSecondE[A,B,C,E](f: B => E \/ C)(t: (A,B)): (E \/ (A,C)) = t match {
    case (a,b) => f(b) map {(a,_)}
  }
  def mapE[A,B,E](ts: List[A])(f: A => E \/ B): (E \/ List[B]) = 
    ts match {
      case Nil => List.empty.right
      case x :: xs => f(x) match {
	case l@ -\/(_) => l
	case \/-(r) => mapE(xs)(f) map {r :: _}
      }
    } // 千切れる

  def id[A](a: A): A = a

}

trait DoAny extends UT {

  def lookup(jv: JValue)(jkey: JValue): Option[JValue] = 
    jkey match {
      case JString(key) => 
	lookupE(jv, key) match {
	  case -\/(KeyNotFound(k)) => Log.warn("missing field. '"+ k +"'");None
	  case -\/(UnexpectedValueType) => Log.error("unexpected value "+ compact(render(jv)));None
	  case -\/(err) => throw new Exception("undefined error code "+ err)
	  case \/-((k,v)) => v.some
	}
      case otherwise => 
	Log.error("unexpected value"+ compact(render(jv)))
        None
    }
  def lookup(jv: JValue, key: String): Option[JValue] = 
    jv findField {case (k,_) => k == key} match {
      case None => None
      case Some((k,v)) => v.some
    }

  def optToE[L,R](orElse: L)(o: Option[R]): \/[L,R] =
    o match {
      case Some(r) => r.right
      case None => orElse.left
    }
  def lookupE(jv: JValue)(jkey: JValue): \/[Error,JField] =
    jkey match {
      case JString(key) => lookupE(jv, key)
      case _ => UnexpectedValueType.left
    }
  def lookupE(jv: JValue, key: String): \/[Error,JField] =
    jv match {
      case JObject(fs) => optToE(KeyNotFound(key)){fs find {case (k,_) => k == key}}
      case _ => UnexpectedValueType.left
    }
}


object Transform extends Loader with UT {
  var baseDir = new File("./")
  var destDir = new File(baseDir, "transformed")

  def bind[A,B](f: A => B)(a: A): (A,B) = (a,f (a))

  def save(x: (File, JValue)) = x match {
    case (f,v) =>
      pretty(MorePretty.prend(v), new FileWriter(f)).close()
  }
  def loadOption(f: File): Option[JValue] = 
    f.exists match {
      case true => load(f).some
      case false => None
    }

  private def in_transform(base: File, dest: File): Unit = {
    if (! dest.exists) {dest.mkdir()}
    main_transform(
      base.listFiles() flatMap {
	f =>
	  if (f isDirectory) { 
	    in_transform(f, new File(dest, f getName))
	    None
	  }
	  else if ( (!(f.getName startsWith "__")) && (f.getName.toLowerCase endsWith ".json" )) {
	    f.some
	  } else {
	    None
	  }
      } toList,
      dest)
  }
  private def main_transform(fs: List[File], dest: File): Unit = {
    fs map { bind(load) } map {
      loadOption(new File(baseDir, "__template.json")) match {
	case Some(templates) => 
	  TransformTemplete(templates) _
	case None =>
	  id _
      }
    } map {
      loadOption(new File(baseDir, "__replace.json")) match {
	case Some(replaceRule) =>
	  Replace(replaceRule) _
	case None =>
	  id _
      }
    } map { CutDoubleUnderscoreFields(_) } map { ChangePath(dest) } foreach (save)
  }

  def transform() {
    in_transform(baseDir, destDir)
/*
    val fs = baseDir listFiles (
      new FilenameFilter {
	override def accept(f: File, name: String) =
	  (!(name startsWith "__")) && ((name.toLowerCase) endsWith ".json")
      })
    
    if (! destDir.exists) {destDir.mkdir()}
    fs map { bind(load) } map {
      loadOption(new File(baseDir, "__template.json")) match {
	case Some(templates) => 
	  TransformTemplete(templates) _
	case None =>
	  id _
      }
    } map {
      loadOption(new File(baseDir, "__replace.json")) match {
	case Some(replaceRule) =>
	  Replace(replaceRule) _
	case None =>
	  id _
      }
    } map { CutDoubleUnderscoreFields(_) } map { ChangePath(destDir) } foreach (save)
// */
  }
}



object TransformTemplete extends DoAny {

  def dropTemplates(jv: JValue): JValue =
    jv removeField {case (k,_) => k == "__templates"}

  def mkInstance(templates: JValue)(incompleteJSON: JValue): JValue = {
    lookup(incompleteJSON, "__templates") match {
      case None => incompleteJSON
      case Some(JArray(vs)) => (
	incompleteJSON /: ( vs flatMap {lookup(templates)} map {mkInstance(templates)}) 
      )((a,b) => a merge b)
      case _ => throw new Exception("fatal")
    }
  }
  def build(templates: JValue)(incompleteJSONs: JValue): JValue = {
    incompleteJSONs match {
      case JArray(js) =>
	JArray(js map {mkInstance(templates)} map { dropTemplates })
      case jv: JObject =>
	dropTemplates( mkInstance(templates)(jv) )
      case _ => throw new Exception("unexpected error")
    }
  }


  def apply(templates: JValue)(arg: (File, JValue)): (File, JValue) =
    mapRight(build(templates))(arg)

}

object Replace extends DoAny {
  
  def buildRule(rule: JValue): Map[String, JValue] =
    rule match {
      case JObject(kvs) => kvs flatMap {
	case (k,v) => (k -> v).some
	case _ => None
      } toMap
      case _ => Map.empty
    }

  def headRightOrList[A,B](xs: List[A \/ B]): (List[A] \/ B) =
    ((List.empty[A].left[B]) /: xs){
      case (-\/(l), -\/(e)) => (e :: l).left[B]
      case (-\/(_), r@ \/-(_)) => r
      case (r@ \/-(_), _) => r
    }

  def replace(rule: Map[String,JValue])(target: JValue): JValue = {
    target match {
      case JObject(kvs) =>
	JObject(
	  kvs map {mapRight{
	    case js@JString(s) => rule get s match {
	      case Some(JArray(rs)) => headRightOrList{rs map {lookupE(kvs)}} match {
		case -\/(es) => Log.warn(s"'$s' replacement failed: "+{es.reverse.mkString(", ")})
		  js
		case \/-((k,v)) => v
	      }
	      case Some(r) => {lookupE(kvs)(r)} match {
		case -\/(e) => Log.warn(s"'$s' replacement failed: "+e.toString)
		  js
		case \/-((k,v)) => v
	      }
	      case None => js
	    }
	    case x => x
	  }})
      case JArray(js) => JArray( js map {replace(rule)} )
      case x => Log.debug(x.toString)
      throw new Exception("unexpected error")
    }
  }
  def apply(rule: JValue)(arg: (File, JValue)): (File, JValue) =
    mapRight{replace( buildRule(rule) ) }(arg)

}

object CutDoubleUnderscoreFields extends DoAny {
  def cutD(jv: JValue): JValue = jv match {
    case JObject(fs) =>
      JObject(fs filterNot {case (k,_) => k startsWith "__"})
    case JArray(vs) => JArray( vs map cutD )
    case _ =>
      throw new Exception("unexpected error")
  }
  def apply(arg: (File, JValue)): (File, JValue) =
    mapRight(cutD)(arg)
}

object ChangePath extends DoAny {
  def chPath(destD: File)(curF: File): File = {
    new File(destD, curF getName)
  }

  def apply(destDir: File)(arg: (File, JValue)): (File, JValue) =
    mapLeft(chPath(destDir))(arg)
}
