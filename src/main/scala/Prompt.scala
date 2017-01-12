
package cddamod

import scala.language.postfixOps
import scalaz._
import scalaz.Scalaz._

import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.Printer.{pretty}

import java.io.File

import scala.io.StdIn

trait Query
object BadQuery extends Query
trait DictionaryFilter extends Query 
object LookupByName extends DictionaryFilter
object LookupAny extends DictionaryFilter
case class Lookup(str: String) extends DictionaryFilter
trait JObjectFilter extends Query {
  def apply(fs: List[JField]): Boolean
}
case class Not(f: JObjectFilter) extends JObjectFilter {
  override def apply(fs: List[JField]) = ! f.apply(fs)
}
case class HasKey(str: String) extends JObjectFilter {
  override def apply(fs: List[JField]) = 
    fs exists {case (k,_) => k == str}
}
case class HasField(key: String, value: JValue) extends JObjectFilter {
  override def apply(fs: List[JField]) =
    fs exists {case (k,v) => k == key && v == value}
}
case class HasValue(value: JValue) extends JObjectFilter {
  override def apply(fs: List[JField]) =
    fs exists {case (_,v) => v == value}
}
trait DisplayRule extends Query
object DisplayPretty extends DisplayRule
object DisplayCompact extends DisplayRule
object DisplayTranslated extends DisplayRule
object DisplayFileName extends DisplayRule
case class DisplayUpTo(upto: Int) extends DisplayRule
trait ResultTransform extends Query {
  def apply(ji: JInfo): Option[JInfo]
}
case class ReturnValue(key: String) extends ResultTransform {
  override def apply(ji: JInfo) =
    ji.jv findField {case (k,_) => k == key} map {case (_,v) => JInfo(v, ji.file)}
}



class Prompt(_browser: Option[String] = None, _dictionary: Option[String] = None) extends DoAny with Loader {
  val browser: Option[Browser] =
    _browser map {new File(_)} orElse Configuration.cddaRoot map BrowserLoader.loadBrowser
  val dictionary: Option[Dictionary] =
    _dictionary map {new File(_)} orElse Configuration.poPath map DictLoader.load

  import prompterror._

  def parseQueryString(input: String): (Error \/ Queries) =
    ws split input toList match {
        case "lookup" :: xs => {LookupAny +: DisplayPretty +: unl(xs)} right
        case "find" :: xs => {DisplayPretty +: unl(xs)} right
        case _ => NoSuchCommand.left
      }

  def execQuery(input: String): (Error \/ List[JInfo]) =
    for {
      qs <- parseQueryString(input)
      ofss <- getOfss(qs)
      jis <- toJis(qs, ofss)
      d <- display( qs.drs )( jis )
    } yield { d }

  def getOfss(qs: Queries): (PromptError \/ List[List[JObjectFilter]]) =
    if ( qs.dfs isEmpty ) {
      List( qs.ofs ).right // (Blank, qs.ofs)
    } else {
      fetchPages( qs.dfs ) map {
        _ map {
          case (de, str) =>
            HasValue(JString(str)) :: qs.ofs // (de, HasValue() :: qs.ofs)
        }
      }
    }
  def toJis(qs: Queries, ofss: List[List[JObjectFilter]]): (Error \/ List[JInfo]) =
    ofss mapE search map {_ flatMap {_ flatMap {filtered( qs.rts )}}}

  def fetchPages(dfs: List[DictionaryFilter]): (PromptError \/ List[(DictionaryElement, String)]) =
    dictionary match {
      case None => NoDictionary.left
      case Some(d) =>
        val orders = dfs flatMap {
	  case Lookup(str) => str.some
	  case _ => None
        }
        if (orders nonEmpty) {
	  if (dfs contains LookupAny) { d lookup (orders.head) right }
	  else if (dfs contains LookupByName) { d nameLookup (orders.head) right }
	  else { InvalidQueryFormat.left }
        } else NoDictionaryOrder.left
    }

  def search(ofs: List[JObjectFilter]): (PromptError \/ Set[JInfo]) =
    browser match {
      case None => NoBrowser.left
      case Some(b) => b lookupXsf ofs right
    }
  def filtered(rts: List[ResultTransform])(ji: JInfo): Option[JInfo] =
    if (rts isEmpty) {
      ji.some
    } else {
      rts flatMap {_.apply(ji)} headOption
    }
    // 複数適用ってどうする？ 今は特定のfieldを抽出してるだけだけど

  def display(drs: List[DisplayRule])(js: List[JInfo]): (Error \/ List[JInfo]) = {
    val num = {drs flatMap {
      case DisplayUpTo(n) => n.some
      case _ => None
    } headOption} getOrElse 25
    js take num map {
      ji =>
      if (drs contains DisplayFileName) {
        println( Configuration cddaRelativePath ji.file )
      }
      if (drs contains DisplayPretty) {
        println( prend(ji.jv) )
      } else if (drs contains DisplayCompact) {
        println( crend(ji.jv) )
      }
      if (drs contains DisplayTranslated) {
        dictionary foreach {
          d =>
          println( prend( d translate ji.jv ) )
        }
      }
    }
    js right
  }

  trait Queries {
    var dfs: List[DictionaryFilter] = List.empty
    var ofs: List[JObjectFilter] = List.empty
    var drs: List[DisplayRule] = List.empty   // これSetにしたくない？
    var rts: List[ResultTransform] = List.empty
    var bad: Option[Query] = None

    def not: this.type = {
      ofs match {
	case x :: xs => ofs = Not(x) :: xs
	case _ => throw new Exception("ofs")
      }
      this
    }
    def +:(q: Query): this.type = {
      q match {
	case df: DictionaryFilter =>
	  df match {
	    case LookupByName =>
	      dfs = df :: {dfs dropWhile(_ == LookupAny)}
	    case LookupAny =>
	      if (!(dfs contains LookupByName)) 
		dfs = df :: dfs
	    case _ =>
	      dfs = df :: dfs
	  }
	case of: JObjectFilter =>
	  ofs = of :: ofs
	case dr: DisplayRule =>
	  dr match {
	    case DisplayCompact =>
	      drs = dr :: (drs dropWhile (_ == DisplayPretty))
	    case DisplayPretty =>
	      if (!(drs contains DisplayCompact))
		drs = dr :: drs
	    case _ => 
	      drs = dr :: drs
	  }
	case rt: ResultTransform =>
	  rts = rt :: rts
	case BadQuery =>
	  bad = BadQuery.some
      }
      this
    }
  }
  object Queries {
    def empty: Queries = new Queries {}
  }
  
  def unl(coms: List[String]): Queries = {
    coms match {
      case Nil => Queries.empty
      case "no" :: xs => unl(xs).not
      case "name" :: xs => LookupByName +: unl(xs)
      case "item" :: xs => HasKey("volume") +: unl(xs)
//      case "recipe" :: xs => HasField("type", JString("recipe")) +: unl(xs)
//    レシピを見に行くには、dictからnameを貰った上で、nameでitemを探して、そのitemのidでrecipeを探さないといけない
      case "id" :: xs => DisplayCompact +: ReturnValue("id") +: unl(xs)
      case "translate" :: xs => DisplayTranslated +: unl(xs)
      case "show" :: "path" :: xs => DisplayFileName +: unl(xs)
      case "up" :: "to" :: num :: xs => 
	try {
	  DisplayUpTo(num.toInt) +: unl(xs)
	} catch {
	  case e: NumberFormatException =>
	    Log.error("format error: " +num)
	    BadQuery +: Queries.empty
	}
      case pKeyValue(key,value) :: xs => HasField(key, JString(value)) +: unl(xs)
        // v の中身をparseして部分検索すると便利そう？
      case pKey(key) :: xs => HasKey(key) +: unl(xs)
      case pValue(value) :: xs => HasValue(JString(value)) +: unl(xs)
      case str :: xs => Lookup(str) +: unl(xs)
    }
  }
  val pKeyValue = """([^\s]+)=([^\s]+)""".r
  val pKey = """([^\s]+)=""".r
  val pValue = """=([^\s]+)""".r
  // ()=?() partial match order??


  def consoleEncoding = Configuration.consoleEncoding
  def wrappedPrompt() {
    import scala.Console
    import java.io.{InputStreamReader, PrintStream}
    import java.nio.charset.Charset

    Console.withOut(new PrintStream(System.out, false, consoleEncoding)){
      Console.withIn(new InputStreamReader(System.in, Charset.forName(consoleEncoding))){
	prompt()
      }}
  }
  
  def prompt() {
    print("> ")
    StdIn readLine match {
      case "exit" => // do nothing
      case "help" => 
        println( help mkString ("","\n","") )
        prompt()
      case any =>
	execQuery(any) match {
	  case -\/(e) =>
	    Log.error( e.toString )
	  case _ => // do nothing
	}
      prompt()
    }
    return
  }
  def prend(jv: JValue): String = pretty(MorePretty.prend(jv))//pretty(render(jv))
  def crend(jv: JValue): String = compact(render(jv))

  val ws = """\s+""".r

  val help = "*help*" ::
  "COMMANDS" ::
  " lookup [options] <string>  入力文字列でpoファイルを逆引きし、" :: 
  "                            その値を持つjsonオブジェクト一覧を表示する" ::
  " find [options]             po逆引きを行わないモード" :: 
  " exit                       終わる" ::
  "" ::
  "" ::
  "OPTIONS" ::
  " up to <number>  検索結果の最大表示数を変更する デフォルトでは25" ::
  " id              見つけたjsonオブジェクトのidの値のみを表示する" ::
  " translate       jsonオブジェクト内の文字列を翻訳して出力する" ::
  " show path       そのjsonオブジェクトがどのfileにあったかを表示する" ::
  "" ::
  "QUANTIIFIER" :: 
  " <key>=<string>  検索結果を特定のキーと値の組を持つjsonオブジェクトに限定する" ::
  " <key>=             〃  を   特定のキー   を持つ        〃           " ::
  " =<string>          〃  を   特定の値     を持つ        〃           " ::
  " name            poファイル内の名前っぽいもののみを対象にする" ::
  " item            検索対象をアイテムに限定する (volume=)と等価" ::
  " no <quantifier> 直後の限定子を否定する" ::
  Nil

}

