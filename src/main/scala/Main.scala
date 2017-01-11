
package cddamod

import scala.language.postfixOps
import scalaz.Scalaz._
import scalaz._

import org.json4s._
import org.json4s.native.JsonMethods._

import java.io.{File, FileFilter}
import java.io.{InputStreamReader, FileInputStream} 
import java.nio.charset.StandardCharsets

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
 *
 * そのjsonオブジェクトがどのfileにあったかも表示したい
 *
 *
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
      parse(new InputStreamReader(new FileInputStream(jsonfile), StandardCharsets.UTF_8))
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

  def couple[A,B](f: A => B)(a: A): (A,B) =
    (a, f (a))

  def recursiveLoad(dir: File): List[JValue] = 
    listFileRcursive(dir) map (load _) flatMap unscribe

  def listAllJValuesWithFile(dir: File): List[(JValue, File)] =
    listFileRcursive(dir) map {couple(load)} flatMap hezy map {_.swap}

  def hezy(fj: (File, JValue)): List[(File, JValue)] =
    fj match {
      case (f, JArray(vs)) => vs map {(f, _)}
      case _ => List(fj)
    }

  def unscribe(jv: JValue): List[JValue] = {
    jv match {
      case JArray(js) => js
      case o => List(o)
    }
  }

}

object Main extends Loader with DoAny {


  trait Mode
  object Browse extends Mode
  object Trans extends Mode
  object Po extends Mode
  private[this] def repf(mode: Mode, args: List[String], target: Option[String]) {
    args match {
      case "-w" :: v :: xs => repf(mode, xs, v.some)
      case "-b" :: xs => repf(Browse, xs, target)
      case "-p" :: xs => repf(Po, xs, target)
      case "--help" :: _ => showHelp()
      case "-h" :: _ => showHelp()
      case a :: b :: Nil =>
	Configuration.load(target)
        mode match {
	  case Browse =>
            new Prompt(a.some, b.some).wrappedPrompt()
	  case Trans =>
            new Transform(a, b.some).transform()
	  case Po =>
	    TranslationHelper.build( new File(a), new File(b) )
	}
      case a :: Nil =>
        Configuration.load(target)
        mode match {
	  case Browse =>
            new Prompt(a.some).wrappedPrompt()
	  case Trans =>
            new Transform(a).transform()
	  case Po =>
	    val dir = new File(a)
	    TranslationHelper.build( dir, new File({dir getName} + ".po"))
	}
      case Nil =>
        Configuration.load(target)
        mode match {
	  case Browse =>
	    new Prompt().wrappedPrompt()
	  case Trans =>
            Configuration.source match {
              case Some(a) =>
                new Transform(a).transform()
              case None =>
                showHelp()
            }
	case Po =>
	  showHelp()
      }
      case _ => showHelp()
    }
  }
  def showHelp() {
    val help = "*usage*" ::
    "transform   :> [options] <sourceDirecotry> [destDirecotry]" ::
    "browser     :> -b [options] [jsonRootDirectory [poFile]]" ::
    "make po file:> -p <targetModPath> [outFile]" ::
    "" ::
    "OPTION" ::
    "    -w <targetVersion>" ::
    Nil
    println(help mkString("","\n",""))
  }

  def main(args: Array[String]) {
    repf(Trans, args toList, None)
  }

}

