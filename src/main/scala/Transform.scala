
package cddamod

import scala.language.postfixOps
import scala.language.higherKinds
import scala.language.implicitConversions
import scalaz._
import scalaz.Scalaz._

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._
import org.json4s.native.Printer.{pretty}

import java.io.{File, FileWriter}

trait Error
case class KeyNotFound(key: String) extends Error {
  override def toString = "KeyNotFound[\""+key+"\"]"
}
object UnexpectedValueType extends Error {
  override def toString = "UnexpectedValueType"
}
case class ExpectedValueType(str: String, actual: JValue) extends Error
case class FieldNotFound(key: String, at: JValue) extends Error

trait UT {
  def first[A,B](t: (A,B)): A = t._1
  def second[A,B](t: (A,B)): B = t._2

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

  private[this] def itself = this
  implicit def toUTOp[A](ls: List[A]): UTOp[A] = new UTOp[A] {
    def tool = itself
    def self = ls
  } 
}

trait UTOp[A] {
  protected[this] def tool: UT
  protected[this] def self: List[A]
  def mapD[B,E](f: A => E \/ B): (E \/ List[B]) =
    tool.mapE(self)(f)
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
      case _ => ExpectedValueType("JString", jkey).left
    }
  def lookupE(jv: JValue, key: String): \/[Error,JField] =
    jv match {
      case JObject(fs) => optToE(KeyNotFound(key)){fs find {case (k,_) => k == key}}
      case _ => ExpectedValueType(s"JObject {$key}", jv).left
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

  private def in_transform(base: File, dest: File): Unit = {
    if (! dest.exists) {dest.mkdir()} // 関係ないフォルダもつくっちゃう
    main_transform(
      base.listFiles() filter {_ != dest} flatMap {
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
      mapRight(ImportObject.build)
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
  }
}



object TransformTemplete extends DoAny {

  def dropTemplates(jv: JValue): JValue =
    jv removeField {case (k,_) => k == "__templates"}

  def mkInstance(templates: JValue)(incompleteJSON: JValue): JValue = {
    lookup(incompleteJSON, "__templates") match {
      case None => incompleteJSON
      case Some(JArray(vs)) => (
        ( vs flatMap {lookup(templates)} map {mkInstance(templates)} ) :\ incompleteJSON
      )( _ merge _)
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

object ImportObject extends DoAny {
  var browser: Option[Browser] = None

  trait ImportError extends Error
  object NoBrowser extends ImportError
  object RefNotFound extends ImportError
  object MultiRef extends ImportError
  object NoBindIdent extends ImportError
  object Reserved extends ImportError
  case class FormatError(opt: String) extends ImportError
  case class NotFoundIdent(ident: String) extends ImportError
  case class NoSupport(code: String) extends ImportError

  private[this] def mkQuery(fs: List[JField]): List[JObjectFilter] =
    fs map {
      case (key, JString("__undefined")) => Not(HasKey(key))
      case (key, value) => HasField(key, value)
    }
  
  case class ImportedObject(val instance: JValue, val rule: String, val ident: Option[String])

  def mkObject(jv: JValue): (Error \/ ImportedObject) = 
    for {
      is <- lookup(jv, "ignore") match {
	case None => List.empty[String].right
	case Some(JArray(gs)) => gs mapD {
	  case JString(s) => s.right
	  case x => ExpectedValueType("JString", x).left
	}
	case x => ExpectedValueType("JArray", x).left
      }
      i <- lookupE(jv, "ref") map second flatMap {
	case JObject(ref) => mkQuery(ref).right
	case _ => RefNotFound.left
      } flatMap {
	ofs =>
	  browser match {
	    case None => NoBrowser.left
	    case Some(b) => b lookupXs ofs match {
	      case x :: Nil => x.right
	      case _ => MultiRef.left
	    }
	  }
      } flatMap {
	case JObject(fs) => 
	  val missingKeys = is.toSet -- {fs map first toSet}
	  if (missingKeys isEmpty) {
            JObject ( fs filterNot {case (k,_) => is contains k} ) right
	  } else {
	    KeyNotFound(missingKeys mkString ",").left
	  }
	case x => ExpectedValueType("JObject", x).left
      }
      r <- lookupE(jv, "import") map second flatMap {
	case JString(str) => str.right
	case x => ExpectedValueType("JString", x).left
      }
      b <- lookupE(jv, "bind") map second match {
	case -\/(_) => r match {
	  case "as template" => None.right
	  case "bind only" => NoBindIdent.left
	  case x => NoSupport(x).left
	}
	case \/-(x) => x match {
	  case JString("this") => Reserved.left
	  case JString(s) => s.some.right
	  case x => ExpectedValueType("JString", x).left
	}
      }
    } yield {ImportedObject(i,r,b)}
  
  
  case class ParseError(failureString: String, atWord: String) extends Error
  val uofe = """([^\d\s][^\s\.]*)\.([^\d\s][^\s]*)""".r
  val ws = """\s+""".r
  trait Atom
  trait Star extends Atom
  trait Op extends Atom {
    def invoke(v1: JValue, v2: JValue): (Error \/ JValue)
  }
  case class Ref(a: String, b: String) extends Star
  case class This(b: String) extends Star
  object Add extends Op {
    override def invoke(v1: JValue, v2: JValue): (Error \/ JValue) =
      (v1,v2) match {
	case (JInt(l), JInt(r)) => JInt(l + r).right
	case (JInt(l), JDouble(r)) => JInt({l.toDouble + r}.toInt).right
	case (JDouble(l), JInt(r)) => JDouble(l + r.toDouble).right
	case (JDouble(l), JDouble(r)) => JDouble(l + r).right
	case _ => throw new Exception("JI,JD "+ v1 + ", " + v2)
      }
  }
  object Sub extends Op {
    override def invoke(v1: JValue, v2: JValue): (Error \/ JValue) =
      (v1,v2) match {
	case (JInt(l), JInt(r)) => JInt(l - r).right
	case (JInt(l), JDouble(r)) => JInt({l.toDouble - r}.toInt).right
	case (JDouble(l), JInt(r)) => JDouble(l - r.toDouble).right
	case (JDouble(l), JDouble(r)) => JDouble(l - r).right
	case _ => throw new Exception("JI,JD "+ v1 + ", " + v2)
      }
  }
  object Times extends Op {
    override def invoke(v1: JValue, v2: JValue): (Error \/ JValue) =
      (v1,v2) match {
	case (JInt(l), JInt(r)) => JInt(l * r).right
	case (JInt(l), JDouble(r)) => JInt({l.toDouble * r}.toInt).right
	case (JDouble(l), JInt(r)) => JDouble(l * r.toDouble).right
	case (JDouble(l), JDouble(r)) => JDouble(l * r).right
	case _ => throw new Exception("JI,JD "+ v1 + ", " + v2)
      }
  }
  object Append extends Op {
    override def invoke(v1: JValue, v2: JValue): (Error \/ JValue) =
      (v1,v2) match {
	case (JArray(l), JArray(r)) => JArray(l ++ r).right
	case _ => UnexpectedValueType.left
      }
  }
  case class Box(jv: JValue) extends Star
  val decimal = """(\d+)""".r
  val float = """(\d+\.\d+)""".r
  def splitString(str: String): (Error \/ List[Atom]) = {
    {ws split str toList} mapD {
      case uofe("this", r) => This(r).right
      case uofe(l,r) => Ref(l,r).right
      case "+" => Add.right
      case "-" => Sub.right
      case "*" => Times.right
      case "++" => Append.right
      case decimal(n) => Box(JInt(n.toInt)).right
      case float(n) => Box(JDouble(n.toDouble)).right
      case xx => ParseError(str, xx).left
    }
  }
  def calc(self: JValue, ios: List[ImportedObject], ls: List[Atom]): (Error \/ JValue) = {
    val getStar = unstar(self,ios) _
    ls match {
      case (s1: Star) :: (op: Op) :: (s2: Star) :: ss => 
	getStar(s1) flatMap {
	  v1 => getStar(s2) flatMap {
	    v2 => op.invoke(v1,v2) flatMap {
	      v => calc(self,ios, Box(v) :: ss)
	    }
	  }
	}
      case (s: Star) :: Nil => getStar(s)
      case _ => FormatError("calc").left
    }
  }
  private[this] def unstar(self: JValue, ios: List[ImportedObject])(s: Star): (Error \/ JValue) = {
    s match {
      case Box(jv) => jv.right
      case This(r) => lookupE(self, r) map second
      case Ref(a,b) => optToE(NotFoundIdent(a)){ ios find {_.ident contains a}} flatMap {
	case ImportedObject(jv, _, _) => lookupE(jv, b) map second
      }
      case _ => throw new Exception("unexpected error")
    }
  }

  def build(jv: JValue): JValue = {
    jv match {
      case jo@ JObject(fs) => 
	{lookup(jo, "__import") match {
	  case None => None
	  case Some(JArray(vs)) => vs mapD mkObject some
	  case _ => FormatError("__import").left.some
	}} match {
	  case None => jo
	  case Some(ux) => ux flatMap {
	    ios =>
	      fs mapD {fSecondE{
		case JString(target(s)) => 
		  splitString(s) flatMap {calc(jo, ios, _)}
		case jv => jv.right
	      }} map {JObject(_)} map {
		v: JValue => 
		  val l = ios filter {
		    case ImportedObject(_,"as template",_) => true
		    case _ => false
		  } map {
		    case ImportedObject(i, _, _) => i
		  }
		(l :\ v)( _ merge _ )}
	  } match {
	    case -\/(e) => Log.error(e.toString); JObject(Nil)
	    case \/-(r) => r
	  }
	}
      case JArray(vs) => JArray(vs map build)
      case x => x
    }
  }
  val target = """[$]\{(.*)\}""".r
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
