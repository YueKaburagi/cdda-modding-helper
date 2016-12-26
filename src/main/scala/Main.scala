
package cddamod

import scala.language.postfixOps
import scalaz.Scalaz._
import scalaz._

import org.json4s._
import org.json4s.native.JsonMethods._

import java.io.{File, FileFilter, FileWriter, BufferedWriter}
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
 * 英語modを日本語訳したい？
 *  1. .pot を出力して .po を経て .mo これを本体同梱の .mo へマージ？
 *  2. jsonに原文一覧を抽出、それを編集してもらって、原文上書きしたjsonを出力？
 *  原文一覧の抽出までは共通、pot を作るのはテキストベタ書きでだいじょうぶかな？
 * ${arr -- arr}こういうことしたいんだけど、右辺項どうやって求めよう
// */

object TranslationHelper extends Loader {
  def build(path: File, out: File) {
    val writer = new BufferedWriter( new FileWriter( out ))
    recursiveLoad(path) map listupText map {_ toMap} flatMap toPoLikeString foreach {
      str: String =>
	writer write str
        writer write "\n\n"
    }
    writer flush
  }

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

  val candidates: Set[String] = Set("name", "name_plural", "description",
				    // a-able item msg
				    "msg", "not_ready_msg", "unfold_msg", 
				    // npc talk
				    "dynamic_line", "responses", 
				    "yes", "no", "text", "npc_male", "npc_female", "u_male", "u_female"
				  )
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
	lookup(jv, "console_encoding") map {
	  case JString(s) => Prompt.consoleEncoding = s
	  case _ => // do nothing
	}
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
  object Po extends Mode
  private[this] def repf(mode: Mode, args: List[String], target: Option[String]) {
    args match {
      case "-w" :: v :: xs => repf(mode, xs, v.some)
      case "-b" :: xs => repf(Browse, xs, target)
      case "-p" :: xs => repf(Po, xs, target)
      case "--help" :: _ => showHelp()
      case "-h" :: _ => showHelp()
      case a :: b :: Nil => mode match {
	case Browse =>
	  loadConfig(target)
	  Prompt.browser = new Browser(recursiveLoad(new File( a ))).some
          Prompt.dictionary = DictLoader.load(new File( b )).some
	  
	  Prompt.wrappedPrompt()
	case Trans =>
	  loadConfig(target)
	  Transform.baseDir = new File( a )
	  Transform.destDir = new File( b )
	  Transform.transform()
	case Po =>
	  TranslationHelper.build( new File(a), new File(b) )
	}
      case a :: Nil => mode match {
	case Browse =>
	  loadConfig(target)
	  Prompt.browser = new Browser(recursiveLoad(new File( a ))).some
	  Prompt.wrappedPrompt()
	case Trans =>
	  loadConfig(target)
	  Transform.baseDir = new File( a )
	  Transform.destDir = new File( Transform.destDir, Transform.baseDir.getName )
	  Transform.transform()
	case Po =>
	  val dir = new File(a)
	  TranslationHelper.build( dir, new File({dir getName} + ".po"))
	}
      case Nil => mode match {
	case Browse =>
	  loadConfig(target)
	  Prompt.wrappedPrompt()
	case Trans =>
	  showHelp()
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

