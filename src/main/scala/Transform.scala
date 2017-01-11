
package cddamod

import scala.language.postfixOps
import scalaz._
import scalaz.Scalaz._

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._
import org.json4s.native.Printer.{pretty}
//import org.json4s.scalaz.JsonScalaz._

import java.io.{File, FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets


class Transform(_source: String, _dest: Option[String] = None) extends Loader with UT {
  val srcDir = new File(_source)
  val destDir =
    new File(_dest getOrElse Configuration.destination, srcDir.getName)

  def save(x: (File, JValue)) = x match {
    case (f,v) =>
      pretty(MorePretty.prend(v), 
	     new OutputStreamWriter(new FileOutputStream(f), StandardCharsets.UTF_8) ).close()
  }

  private def targetList(base: File, dest: File): List[(List[File], File)] =
    base.listFiles() filter {_ != dest} partition (_.isDirectory) match {
      case (dirs, files) =>
	lazy val rs: List[(List[File], File)] =
	  {dirs flatMap {b => targetList(b, new File(dest, b.getName))} toList}
	files filter {
	  _.getName.toLowerCase endsWith ".json"
	} filterNot {
	  _.getName startsWith "__"
	} match {
	  case ls if ls isEmpty => rs
	  case ls => (ls toList, dest) :: rs
	}
    }
  private def in_transform(base: File, dest: File): Unit = {
    targetList(base, dest) map {
      case (files, destdir) =>
	if (! destdir.exists) {destdir.mkdirs()}
	main_transform(files, destdir)
    }
  }
  private def main_transform(fs: List[File], dest: File): Unit = {
    fs map { couple(load) } map {
      loadOption(new File(srcDir, "__template.json")) match {
	case Some(templates) => 
	  TransformTemplete(templates) _
	case None =>
	  id _
      }
    } map {
      fSecond(new ImportObject().build)
    } map {
      loadOption(new File(srcDir, "__replace.json")) match {
	case Some(replaceRule) =>
	  Replace(replaceRule) _
	case None =>
	  id _
      }
    } map { CutDoubleUnderscoreFields(_) } map { ChangePath(dest) } foreach (save)
  }

  def transform() {
    in_transform(srcDir getCanonicalFile, destDir getCanonicalFile)
  }
}


object TransformTemplete extends DoAny {

  def dropTemplates(jv: JValue): JValue =
    jv removeField {case (k,_) => k == "__templates"}

  // これ Validation の仕事じゃないかなぁ
  def mkInstance(templates: JValue)(incompleteJSON: JValue): (Error \/ JValue) = {
    incompleteJSON lookup "__templates" match {
      case None => incompleteJSON right
      case Some(JArray(vs)) => 
	vs mapD {
	  k =>
	    lookupE(templates)(k) map second flatMap {mkInstance(templates)}
	} map {ss => (ss :\ incompleteJSON){_ merge _} }
      case x => UnmatchedValueType("[\"\"]", x) left
    }
  }
  def build(templates: JValue)(incompleteJSONs: JValue): JValue = {
    incompleteJSONs match {
      case JArray(js) =>
	JArray(js map {mkInstance(templates)} flatMap {
	  case -\/(e) => 
	    Log.error(e.toString)
	    None
	  case \/-(r) =>
	    r some
	} map { dropTemplates })
      case jv: JObject =>
	mkInstance(templates)(jv) match {
	  case -\/(e) => 
	    Log.error(e.toString)
	    jv
	  case \/-(r) =>
	    dropTemplates(r)
	}
      case x => 
	Log.error(UnmatchedValueType("[\"\"] or {}", x).toString)
	x
    }
  }


  def apply(templates: JValue)(arg: (File, JValue)): (File, JValue) =
    fSecond(build(templates))(arg)

}

class ImportObject(_browser: Option[String] = None) extends DoAny with Loader {
  val browser: Option[Browser] =
    _browser map {new File(_)} orElse Configuration.poPath map {
      f => new Browser(recursiveLoad(f))
    }
  import importerror._

  private[this] def mkQuery(fs: List[JField]): List[JObjectFilter] =
    fs map {
      case (key, JString("__undefined")) => Not(HasKey(key))
      case (key, value) => HasField(key, value)
    }
  
  case class ImportedObject(val instance: JValue, val rule: String, val ident: Option[String])

  def mkObject(jv: JValue): (Error \/ ImportedObject) = 
    for {
      is <- jv lookup "ignore" match {
	case None => List.empty[String].right
	case Some(JArray(gs)) => gs mapD {
	  case JString(s) => s.right
	  case x => ExpectedValueType("JString", x).left
	}
	case x => ExpectedValueType("JArray", x).left
      }
      i <- jv lookupE "ref" flatMap {
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
	    KeyNotFound(missingKeys mkString ",", JObject(fs)).left
	  }
	case x => ExpectedValueType("JObject", x).left
      }
      r <- jv lookupE "import" flatMap {
	case JString(str) => str.right
	case x => ExpectedValueType("JString", x).left
      }
      b <- jv lookupE "bind" match {
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
	case _ => UnexpectedValueType(JObject( ("lhs" -> v1) :: ("rhs" -> v2) :: Nil)).left
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
      case This(r) => self lookupE r
      case Ref(a,b) => optToE(NotFoundIdent(a)){ ios find {_.ident contains a}} flatMap {
	case ImportedObject(jv, _, _) => jv lookupE b
      }
      case _ => throw new Exception("unexpected error")
    }
  }

  def build(jv: JValue): JValue = {
    jv match {
      case jo@ JObject(fs) => 
	{jo lookup "__import" match {
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
	  kvs map {fSecond{
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
    fSecond{replace( buildRule(rule) ) }(arg)

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
    fSecond(cutD)(arg)
}

object ChangePath extends DoAny {
  def chPath(destD: File)(curF: File): File = {
    new File(destD, curF getName)
  }

  def apply(destDir: File)(arg: (File, JValue)): (File, JValue) =
    fFirst(chPath(destDir))(arg)
}
