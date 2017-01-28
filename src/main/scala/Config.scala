
package cddamod

import scalaz._
import scalaz.Scalaz._

import org.json4s._

import java.io.File

object Configuration extends Loader with DoAny {
  // for browser
  var cddaRoot: Option[File] = None
  var poPath: Option[File] = None
  var consoleEncoding: String = "UTF-8"
  // for transform
  var destination: String = "./transformed"
  var source: Option[String] = None
  // for translate
  var translationTargets: Set[String] =
    Set("name", "name_plural", "description",
      // a-able item msg
      "msg", "not_ready_msg", "unfold_msg",
      // npc talk
      "dynamic_line", "responses",
      "yes", "no", "text", "npc_male", "npc_female", "u_male", "u_female"
    )


  def cddaJsonRoot: Option[File] =
    cddaRoot map {f => new File(f, "data/json")}
  def cddaModsRoot: Option[File] =
    cddaRoot map {f => new File(f, "data/mods")}
  def cddaRelativePath(f: File): String =
    f.getCanonicalPath stripPrefix {
      cddaRoot match {
        case Some(file) => file.getCanonicalPath
        case None => ""
      }
    }



  // conffile の 位置によって 幾つかのpath は変化する？
  def load(version: Option[String], confFile: File) {
    loadOption(confFile) foreach {
      jv =>
      jv lookup "console_encoding" foreach {
        case JString(s) => this.consoleEncoding = s
        case o => Log.warn("console_encoding should be string.")
      }
      loadSettings(jv, confFile)
      version orElse {
        jv lookup "default_version" flatMap {
	  case JString(s) => s.some
	  case _ => None
	}
      } map {
	v => jv lookupE v match {
	  case -\/(e) => Log.error(e.toString)
	  case \/-(j) => loadSettings(j, confFile)
	}
      }
    }
  }
  private[this] def newPath(rt: File)(f: String): File =
    if (new File(f).isAbsolute) {
      new File(f)
    } else {
      new File(rt, f).getCanonicalFile
    }
  // confFile に対して relative なのは cdda_root と po_path だけ (試験的変更)
  private[this] def loadSettings(jv: JValue, confFile: File) {
    Option(confFile.getCanonicalFile.getParentFile) foreach {
      dir => 
      jv match {
        case JObject(fs) => fs foreach {
	  case ("cdda_root", JString(cddaRoot)) =>
            // cddaRoot is relative?
            this.cddaRoot = newPath(dir)(cddaRoot).some
	  case ("po_path", JString(poPath)) =>
            this.poPath = newPath(dir)(poPath).some
	  case ("destination", JString(destPath)) =>
            this.destination = destPath 
	  case ("source", JString(srcPath)) =>
            this.source = srcPath.some
          case ("translation_targets", JArray(vs)) =>
            this.translationTargets =
              vs flatMap {
                case JString(s) => s.some
                case _ => None
              } toSet
	  case _ => // do nothing
        }
        case _ => Log.error("format error: \"__confing.json\"")
      }
    }
  }

}

