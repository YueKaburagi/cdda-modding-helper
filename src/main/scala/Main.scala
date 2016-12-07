
package cddamod

import scala.language.postfixOps
import scalaz.Scalaz._

import org.json4s._
import org.json4s.native.JsonMethods._

import java.io.{File, FileFilter}

import scala.io.AnsiColor


object Log {
  var isDebug: Boolean = true 

  def error(msg: String) = defaultOut(AnsiColor.RED, "error", msg)
  def fatal(msg: String) = defaultOut(AnsiColor.BOLD+AnsiColor.MAGENTA, "fatal", msg)
  def warn(msg: String) = defaultOut(AnsiColor.YELLOW, "warn", msg)
  def info(msg: String) = defaultOut(AnsiColor.CYAN, "info", msg)
  def debug(msg: String): Unit = if(isDebug) defaultOut(AnsiColor.GREEN, "debug", msg)
  def debug(cond: => Boolean, msg: String): Unit = if(cond) debug(msg) 
  def debug(cond: => Boolean, msgT: String, msgF: String): Unit =
    if (cond) debug(msgT) else debug(msgF)

  private def defaultOut(prefix: String, code: String, msg: String): Unit = {
    println("["+prefix + code + AnsiColor.RESET+"] "+msg)
  }
}

// とりあえずtemplateは1つということで
/*** ver 1.2.0
 * templateで name:__name としているときに、
 * それを inherit した場所への replace が失敗する
 * というかそこだけmergeに失敗する。 (たぶん型が異なるせい)
 * めんどくさいなー
 * 
 * ブラウザ機能にkeydown検出を使いたくなったけど、java nativeでは無理なので、
 * 保留
 *
 * そのjsonオブジェクトがどのfileにあったかも表示したい
 *
 * __config,json
 * {
 *   "#6001": {
 *     "export_to": "/path/to/cdda/data/mods",
 *     "cdda_json_root": "/path/to/cdda/data/json",
 *     "po_file": "path/to/po_file.po"
 *   }
 * }
 * ${arr -- arr}こういうことしたいんだけど、右辺項どうやって求めよう
// */

class Browser(jsons: List[JValue]) {
  def lookupXs(fs: Seq[JObjectFilter]): List[JValue] =
    jsons filter {v => fs forall {contains(_)(v)}}

  def lookup(f: JObjectFilter): List[JValue] =
    jsons filter {contains(f)}
  
  def contains(f: JObjectFilter)(jv: JValue): Boolean =
    jv match {
      case JObject(fs) => f apply fs
      case _ => false
    }
}

trait Loader {

  def load(jsonfile: File): JValue = 
    try {
      parse(jsonfile)
    } catch {
      case e: ParserUtil.ParseException =>
	Log.error("parsing error on "+ jsonfile)
	JObject(Nil)
    }
  def loadOption(f: File): Option[JValue] = 
    f.exists match {
      case true => load(f).some
      case false => None
    }

  def listFileRcursive(dir: File): List[File] =
    {dir listFiles (
      new FileFilter {
	override def accept(f: File) = 
	  (f.isDirectory) || ((f.getName.toLowerCase) endsWith ".json")
      }) toList} map {
      f: File =>
	if (f.isDirectory) listFileRcursive(f) else List(f)
    } flatten


  def recursiveLoad(dir: File): List[JValue] = 
    listFileRcursive(dir) map (load _) flatMap unscribe 

  def unscribe(jv: JValue): List[JValue] = {
    jv match {
      case JArray(js) => js
      case o => List(o)
    }
  }

}

object Main extends Loader {

  def main(args: Array[String]) {
    loadOption(new File("__config.json")) map { // このあたりLazyでいいと思う
      case JObject(fs) => fs foreach {
	case ("json_root_dir", JString(jsonRootDir)) =>
	  Prompt.browser = new Browser(recursiveLoad(new File(jsonRootDir))).some
	  ImportObject.browser = Prompt.browser
	case ("po_path", JString(poPath)) =>
	  Prompt.dictionary = DictLoader.load(new File(poPath)).some
	case _ => // do nothing
      }
      case _ => Log.error("format error: \"__confing.json\"")
    }

    args match {
      case Array("-b") =>
	Prompt.prompt()
      case Array("-b", jsonDir, poFile) =>
	Prompt.browser = new Browser(recursiveLoad(new File(jsonDir))).some
        Prompt.dictionary = DictLoader.load(new File(poFile)).some
	Prompt.prompt()
      case Array(baseDir, destDir) =>
	Transform.baseDir = new File(baseDir)
	Transform.destDir = new File(destDir)
	Transform.transform()
      case Array(baseDir) =>
	Transform.baseDir = new File(baseDir)
	Transform.destDir = new File(baseDir, "transformed")
	Transform.transform()
      case _ =>
	Transform.transform()
    }
  }

}

