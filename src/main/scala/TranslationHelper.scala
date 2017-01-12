
package cddamod

import scala.language.postfixOps

import org.json4s._

import java.io.{File, FileWriter, BufferedWriter}

object TranslationHelper extends Loader {
  def build(path: File, out: File) {
    val writer = new BufferedWriter( new FileWriter( out ))
    listAllJValuesWithFile(path) map {
      case (jv, f) =>
        val src = "#: " + getRelativePath(path)(f) + "\n"
        toPoLikeString( listupText(jv).toMap )  foreach {
          str: String =>
          writer write src
	  writer write str
          writer write "\n\n"
        }
    }
    writer flush
  }
  private[this] def getRelativePath(d: File)(f: File): String =
    f.getCanonicalPath stripPrefix {d.getParentFile.getCanonicalPath + "/" }

  private[this] def specializePlural(m: Map[String,String]): (Map[String,String], List[String]) =
    m get "name_plural" match {
      case Some(plural) => 
	m get "name" match {
	  case Some(name) => 
	    (m filterKeys {k => "name" != k && "name_plural" != k},
	    {"msgid \"" +name+ "\"\nmsgid_plural \"" +plural+ "\"\nmsgstr[0] \"\""} :: Nil)
	  case None => throw new Exception("Missing name filed")
	}
      case None =>
	(m,Nil)
    }

  def toPoLikeString(m: Map[String,String]): List[String] =
    specializePlural(m) match {
      case (mn, l) =>
	l ++ {mn map {
	  case (_,v) => "msgid \""+v+"\"\nmsgstr \"\""
	}}
    }

  def candidates: Set[String] = Configuration.translationTargets

  // 1ファイルでなく、1つのjoに対して適用する
  def listupText(jv: JValue): List[(String, String)] = {
    jv match {
      case JObject(fs) => 
	fs flatMap {
	  case (k,v) if candidates contains k => v match {
	    case JString(s) => (k, s) :: Nil
	    case JArray(vs) => vs flatMap listupText
	    case jo@ JObject(_) => listupText(jo)
	    case _ => Nil
	  }
	  case (_,v) => v match {
	    case JArray(vs) => vs flatMap listupText
	    case jo@ JObject(_) => listupText(jo)
	    case _ => Nil
	  }
	}
      case JArray(vs) => vs flatMap listupText
      case _ => Nil
    }
  }
}

