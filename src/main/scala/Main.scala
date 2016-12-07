
package cddamod

import scala.language.postfixOps
import scalaz.Scalaz._
import scalaz._

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
 *   "default_version": "#6001",
 *   "#6001": {
 *     "export_to": "/path/to/cdda/data/mods",
 *     "cdda_json_root": "/path/to/cdda/data/json",
 *     "po_file": "path/to/po_file.po"
 *   }
 * }
 *
 * 英語modを日本語訳したい？
 *  1. .pot を出力して .po を経て .mo これを本体同梱の .mo へマージ？
 *  2. jsonに原文一覧を抽出、それを編集してもらって、原文上書きしたjsonを出力？
 *  原文一覧の抽出までは共通、pot を作るのはテキストベタ書きでだいじょうぶかな？
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

object Main extends Loader with DoAny {
  def loadConfig(version: Option[String]) {
    loadOption(new File("__config.json")) map { // このあたりLazyでいいと思う
      jv =>
	{version match {
	  case Some(v) => v.some
	  case None => lookup(jv, "default_version") flatMap {
	    case JString(s) => s.some
	    case _ => None
	  }
	}} match {
	  case None => loadSettings(jv)
	  case Some(v) => lookupE(jv, v) map second match {
	    case -\/(e) => Log.error(e.toString)
	    case \/-(j) => loadSettings(j)
	  }
	}
    }
  }
  private[this] def loadSettings(jv: JValue) {
    jv match {
      case JObject(fs) => fs foreach {
	case ("cdda_json_root", JString(jsonRootDir)) =>
	  Prompt.browser = new Browser(recursiveLoad(new File(jsonRootDir))).some
	  ImportObject.browser = Prompt.browser
	case ("po_path", JString(poPath)) =>
	  Prompt.dictionary = DictLoader.load(new File(poPath)).some
	case ("destination", JString(destPath)) =>
	  Transform.destDir = new File(destPath)
	case _ => // do nothing
      }
      case _ => Log.error("format error: \"__confing.json\"")
    }
  }

  trait Mode
  object Browse extends Mode
  object Trans extends Mode
  private[this] def repf(mode: Mode, args: List[String], target: Option[String]) {
    args match {
      case "-w" :: v :: xs => repf(mode, xs, v.some)
      case "-b" :: xs => repf(Browse, xs, target)
      case "--help" :: _ => showHelp()
      case "-h" :: _ => showHelp()
      case a :: b :: Nil => mode match {
	case Browse =>
	  loadConfig(target)
	  Prompt.browser = new Browser(recursiveLoad(new File( a ))).some
          Prompt.dictionary = DictLoader.load(new File( b )).some
	  Prompt.prompt()
	case Trans =>
	  loadConfig(target)
	  Transform.baseDir = new File( a )
	  Transform.destDir = new File( b )
	  Transform.transform()
	}
      case a :: Nil => mode match {
	case Browse =>
	  loadConfig(target)
	  Prompt.browser = new Browser(recursiveLoad(new File( a ))).some
	  Prompt.prompt()
	case Trans =>
	  loadConfig(target)
	  Transform.baseDir = new File( a )
	  Transform.destDir = new File( Transform.destDir, Transform.baseDir.getName )
	  Transform.transform()
	}
      case Nil => mode match {
	case Browse =>
	  loadConfig(target)
	  Prompt.prompt()
	case Trans =>
	  showHelp()
      }
      case _ => showHelp()
    }
  }
  def showHelp() {
    val help = "*usage*" ::
    ": transform" ::
    "[options] <sourceDirecotry> [destDirecotry]" ::
    ": browser" ::
    "-b [options] [jsonRootDirectory [poFile]]" ::
    "" ::
    "OPTION" ::
    "-w <targetVersion>" ::
    Nil
    println(help mkString("","\n",""))
  }

  def main(args: Array[String]) {
    repf(Trans, args toList, None)
  }

}

