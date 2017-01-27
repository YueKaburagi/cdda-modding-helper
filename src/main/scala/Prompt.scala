
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
object LookupQueryString extends DictionaryFilter

trait JItemFilter extends Query {
  def apply(index: Browser#Index, ji: JInfo): Boolean
}
trait JObjectFilter extends JItemFilter {
  override def apply(id: Browser#Index, ji: JInfo) = { 
    ji.jv match {
      case JObject(fs) => withJObject(fs)
      case _ => false
    }
  }
  def withJObject(fs: List[JField]): Boolean
}
case class Not(f: JItemFilter) extends JItemFilter with JObjectFilter {
  override def apply(id: Browser#Index, ji: JInfo) = ! f.apply(id, ji)
  override def withJObject(fs: List[JField]): Boolean =
    f match {
      case jof: JObjectFilter => ! jof.withJObject(fs)
      case _ => false
    }
}
case class And(fs: Set[JItemFilter]) extends JItemFilter {
  override def apply(id: Browser#Index, ji: JInfo) =
    if (fs isEmpty) {true}
    else {fs forall (_ apply (id,ji))}
}
case class Or(fs: Set[JItemFilter]) extends JItemFilter {
  override def apply(id: Browser#Index, ji: JInfo) =
    if (fs isEmpty) {true}
    else {fs exists (_ apply (id,ji))}
}

case class Belongs(modinfo: ModInfo) extends JItemFilter {
  override def apply(id: Browser#Index, ji: JInfo) =
    Browser.modContains(modinfo)(ji)
}

case class HasKey(str: String) extends JObjectFilter {
  override def withJObject(fs: List[JField]) = 
    fs exists {case (k,_) => k == str}
}
case class HasSuchField(key: String, value: JValue) extends JObjectFilter {
  override def withJObject(fs: List[JField]) =
    fs exists {
      case (k,v) => k == key && v == value
    }
}
case class HasField(key: String, value: String) extends JObjectFilter {
  override def withJObject(fs: List[JField]) =
    fs exists {
      case (k,JString(v)) => k == key && v == value
      case _ => false
    }
}
case class HasValue(value: String) extends JObjectFilter {
  override def withJObject(fs: List[JField]) =
    fs exists {
      case (_,JString(v)) => v == value
      case _ => false
    }
}

case class HasKeyP(str: String) extends JObjectFilter {
  override def withJObject(fs: List[JField]) = 
    fs exists {case (k,_) => k containsSlice str}
}
case class HasFieldPP(key: String, value: String) extends JObjectFilter {
  override def withJObject(fs: List[JField]) =
    fs exists {
      case (k,JString(v)) => (k containsSlice key) && (v containsSlice value)
      case _ => false
    }
}
case class HasFieldXP(key: String, value: String) extends JObjectFilter {
  override def withJObject(fs: List[JField]) =
    fs exists {
      case (k,JString(v)) => (k == key) && (v containsSlice value)
      case _ => false
    }
}
case class HasFieldPX(key: String, value: String) extends JObjectFilter {
  override def withJObject(fs: List[JField]) =
    fs exists {
      case (k,JString(v)) => (k containsSlice key) && (v == value)
      case _ => false
    }
}
case class HasValueP(value: String) extends JObjectFilter {
  override def withJObject(fs: List[JField]) = 
    fs exists {
      case (_,JString(v)) => v containsSlice value
      case _ => false
    }
}
trait SourceFilter extends JItemFilter 
case class ById(index: String) extends SourceFilter {
  override def apply(id: Browser#Index, ji: JInfo) = true
}
case class ByRoot(root: String) extends SourceFilter {
  override def apply(id: Browser#Index, ji: JInfo) = true
  // root ji.file
}
trait PrintMode extends Query
object PrintPretty extends PrintMode
object PrintCompact extends PrintMode
object PrintNum extends PrintMode
object PrintAsJArray extends PrintMode
//object PrintTable extends PrintMode
trait PrintRule extends Query
case class PrintUpTo(upto: Int) extends PrintRule
object PrintAll extends PrintRule

object DisplayFull { // is not DisplayRule
  def apply(ji: JInfo): List[JField] = ji.jv match {
    case JObject(fs) => fs
    case x => List("(value)" -> x)
  }
}
trait DisplayRule extends Query with Function1[JInfo, List[JField]]
object DisplayRaw extends DisplayRule {
  override def apply(ji: JInfo) = ji match {
    case JInfo(_, _, _, raw) =>
      raw match {
        case JObject(fs) => fs
        case x => List("(raw)" -> x)
      }
  }
}
object DisplayFileName extends DisplayRule {
  override def apply(ji: JInfo) = ji match {
    case JInfo(_, f, _, _) =>
      List( "file" -> JString( Configuration cddaRelativePath f ) )
  }
}
case class DisplayThisValue(key: String) extends DisplayRule with DoAny {
  override def apply(ji: JInfo) = ji match {
    case JInfo(jv, f, ix, _) =>
      List(jv lookup key map (key -> _)).flatten
  }
}
object DisplayIndex extends DisplayRule {
  override def apply(ji: JInfo) = ji match {
    case JInfo(_, _, ix, _) =>
      List( "ix" -> JString(ix.toString) )
  }

}
object DisplayForFacadeList extends DisplayRule with DoAny {
  override def apply(ji: JInfo) = ji match {
    case JInfo(jv, _, ix, _) =>
      def s = jv lookup "name"
      def p = jv lookup "id"
      def r = jv lookup "type"
      def s_ = jv lookup "symbol"
      def y_ = jv lookup "sym" map {
        case JInt(n) => JString(n.toChar.toString)
        case x => x
      }
      List( s orElse p orElse r orElse JString("(no name/id/type)").some map ("name" -> _)
        , s_ orElse y_ map ("symbol" -> _)
        , jv lookup "color" map ("color" -> _) ).flatten
  }
}
object DisplayForFacadeJson extends DisplayRule {
  override def apply(ji: JInfo) = ji match {
    case JInfo(jv, _,_,_) =>
      List( "body" -> jv )
  }
}
trait ResultTransform extends Query with Function1[JInfo, JInfo]
case class ResultTranslate(dictionary: Option[Dictionary]) extends ResultTransform {
  override def apply(i: JInfo) = 
    i match {
    case JInfo(jv, f, ix, raw) =>
      dictionary map {d => JInfo(d translate jv, f, ix, raw)} getOrElse i
  }
}
trait SortOrder {val asc: Boolean}
object Desc extends SortOrder { override val asc = false }
object Asc extends SortOrder { override val asc = true }
trait Sorter extends Query with Function1[Seq[JInfo], Seq[JInfo]]
case class SortByValue(key: String, ord: SortOrder = Asc) extends Sorter with DoAny {
  override def apply(src: Seq[JInfo]) = {
    val (y,n) = src partition (_.jv has key)
    val order = {optionOrder(Order.order(JValueOrder order ord.asc))}
    {y.sortBy{ji: JInfo => ji.jv lookup key}(order.toScalaOrdering)} ++ n
  }
} // translate の後に sort したいときは？



class Prompt(_browser: Option[String] = None, _dictionary: Option[String] = None) extends DoAny with Loader {
  val browser: Option[Browser] =
    _browser map {new File(_)} orElse Configuration.cddaRoot map BrowserLoader.loadBrowser
  val dictionary: Option[Dictionary] =
    _dictionary map {new File(_)} orElse Configuration.poPath map DictLoader.load

  import prompterror._

  // どこかで Map[query,result] をキャッシュしときたい
  def parseQueryString(input: String): (Error \/ Queries) =
    ws split input toList match {
        case "lookup" :: xs => {LookupQueryString +: unl(xs)} right
        case "find" :: xs => unl(xs) right
        case _ => NoSuchCommand.left
      }

  def execQuery(input: String): (Error \/ Unit) =
    for {
      qs <- parseQueryString(input)
      ifs <- getIfs(qs)
      jis <- search(ifs)
      st <- sort(qs.sorter)(jis toSeq).right
      pf <- printFormat( qs.displayRules )( jisTransform(qs)(st) ).right
      d <- display( qs.printMode )( pf ).right
    } yield { d }

  def getIfs(qs: Queries): (Error \/ Set[JItemFilter]) =
    if (qs.withTranslate) {
      replaceJObjectFilter( qs.itemFilters )
    } else {
      qs.itemFilters.right
    }

  def replaceJObjectFilter(ifs: Set[JItemFilter]): (Error \/ Set[JItemFilter]) = {
    val (ofs, others) = ifs partition {
      case x: JObjectFilter => true
      case _ => false
    }
    dictionary match {
      case Some(d) =>
        {{ofs map {f: JItemFilter => f match {
          case HasValueP(v) => Or( d lookupS v map {x => HasValue(x)} )
          case HasValue(v) => Or( d findS v map {x => HasValue(x)} )
          case HasFieldPP(k,v) => Or( d lookupS v map {x => HasFieldPX(k,x)} )
          case HasFieldXP(k,v) => Or( d lookupS v map {x => HasField(k,x)} )
          case HasFieldPX(k,v) => Or( d findS v map {x => HasFieldPX(k,x)} )
          case HasField(k,v) => Or( d findS v map  {x => HasField(k,x)})
          case o => o
        }}} |+| others}.right
      case None => NoDictionary.left
    }
  }
  def jisTransform(qs: Queries)(jis: Seq[JInfo]): Seq[JInfo] = {
    def num = qs.printRules.flatMap {
      case PrintUpTo(n) => n.some
      case _ => None
    }.headOption getOrElse 25
    def js = (qs.printRules contains PrintAll) ? (jis) | (jis take num)
    js map {transform(qs.resultTransforms)}
  }

  // ここでソースをチョイスする？
  def search(ifs: Set[JItemFilter]): (PromptError \/ Set[JInfo]) =
    browser match {
      case None => NoBrowser.left
      case Some(b) => {b lookupXsf ifs}.values.toSet.right
    }
  def transform(rts: List[ResultTransform])(ji: JInfo): JInfo =
    (ji /: rts){case (a,b) => b apply a}

  def printFormat(drs: Set[DisplayRule])(js: Seq[JInfo]): Seq[JObject] =
    js map {
      ji =>
      if (drs isEmpty) {
        DisplayFull(ji)
      } else {
        (List.empty[JField] /: drs){case (fs,f) => (f apply ji) |+| fs }
      }
    } map {JObject(_)}

  def sort(sorter: Option[Sorter])(js: Seq[JInfo]): Seq[JInfo] =
    sorter match {
      case Some(s) => s(js)
      case None => js
    }

  def display(pm: PrintMode)(js: Seq[JObject]): Unit = {
    pm match { 
      case PrintPretty =>
        js foreach {jv => println( prend(jv) )} 
      case PrintCompact =>
        js foreach {jv => println( crend(jv) )}
      case PrintAsJArray =>
        println (
          js map crend mkString("[",",","]")
        )
      case PrintNum =>
        println( s"found ${js.size} objects.")
    } // もしかしたら Buffer して 全部書き込んでから flush とする必要があるかも
  }

  trait Queries {
    private[this] var dfs: Option[DictionaryFilter] = None
    private[this] var ifs: Set[JItemFilter] = Set.empty
    private[this] var drs: Set[DisplayRule] = Set.empty
    private[this] var pm: PrintMode = PrintPretty
    private[this] var prs: Set[PrintRule] = Set.empty
    private[this] var rts: List[ResultTransform] = List.empty
    private[this] var srt: Option[Sorter] = None
    var bad: Option[Query] = None

    def dictionaryFilter = dfs
    def itemFilters = ifs
    def displayRules = drs
    def printMode = pm
    def printRules = prs
    def resultTransforms = rts
    def sorter = srt

    def withTranslate: Boolean = dictionaryFilter contains LookupQueryString

    // state 
    var lJIF: Option[JItemFilter] = None

    def not: this.type = {
      lJIF match {
        case Some(jif) => ifs = {ifs filterNot (jif == _)} + Not(jif)
        case None => throw new Exception("not ???")
      }
      this
    }
    def +:(q: Query): this.type = {
      q match {
	case df: DictionaryFilter =>
	  dfs = df.some
	case jif: JItemFilter =>
          lJIF = jif.some
	  ifs = ifs + jif
	case dr: DisplayRule =>
	  drs = drs + dr
	case rt: ResultTransform =>
	  rts = rt :: rts
        case p: PrintMode =>
          pm = p
        case pr: PrintRule =>
          prs = prs + pr
        case s: Sorter =>
          srt = s.some
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
//      case "item" :: xs => HasKey("volume") +: unl(xs)
//      case "recipe" :: xs => HasField("type", JString("recipe")) +: unl(xs)
//    レシピを見に行くには、dictからnameを貰った上で、nameでitemを探して、そのitemのidでrecipeを探さないといけない
      case "show" :: s :: xs => DisplayThisValue(s) +: unl(xs)
      case "translate" :: xs => ResultTranslate(dictionary) +: unl(xs)
      case "disp" :: "path" :: xs => DisplayFileName +: unl(xs)
      case "short" :: xs => PrintCompact +: unl(xs)
      case "sort" :: "by" :: key :: xs => SortByValue(key) +: unl(xs)
      case "sort" :: "asc" :: "by" :: key :: xs => SortByValue(key, Asc) +: unl(xs)
      case "sort" :: "desc" :: "by" :: key :: xs => SortByValue(key, Desc) +: unl(xs)
      case "mod" :: ident :: xs =>
        {browser flatMap {_ lookupByIdent ident} map {Belongs(_)} getOrElse BadQuery} +: unl(xs)
      case "forFacadeList"  :: xs =>
        PrintAsJArray +: DisplayForFacadeList +: DisplayIndex +: unl(xs)
      case "forFacadeRaw" :: xs =>
        PrintPretty +: DisplayRaw +: unl(xs)
      case "forFacadeInfo" :: xs =>
        PrintCompact +: DisplayForFacadeJson +: DisplayFileName +: unl(xs)
      case "num" :: xs => PrintNum +: unl(xs)
      case "all" :: xs => PrintAll +: unl(xs)
      case "up" :: "to" :: num :: xs => 
	try {
	  PrintUpTo(num.toInt) +: unl(xs)
	} catch {
	  case e: NumberFormatException =>
	    Log.error("format error: " +num)
	    BadQuery +: Queries.empty
	}
      case pId(index) :: xs => ById(index) +: unl(xs)
      case pPartialField(key,value) :: xs => HasFieldXP(key, value) +: unl(xs)
      case pKeyValue(key,value) :: xs => HasField(key, value) +: unl(xs)
      case pKey(key) :: xs => HasKey(key) +: unl(xs)
      case pPartialValue(value) :: xs => HasValueP(value) +: unl(xs)
      case pValue(value) :: xs => HasValue(value) +: unl(xs)
      case str :: xs =>
        Log.error("format error] " +str)
        BadQuery +: unl(xs)
    }
  }
  val pKeyValue = """([^\s]+)=([^\s]+)""".r
  val pKey = """([^\s]+)=""".r
  val pValue = """=([^\s]+)""".r
  val pId = """#([^\s]+)""".r
  // partial match 
  val pPartialValue = """=\?([^\s]+)""".r
  val pPartialField = """([^\s]+)=\?([^\s]+)""".r
  // negative !(.*)
  // partial  ?(.*)
  // escape   \(.*)

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
    print("Browser > ")
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
  " lookup [options|quantifier...]" ::
  "           検索前にQUANTIFIERの<key>や<string>を" ::
  "           po逆引きしてから検索するモード" ::
  "           (逆引きに失敗した場合は入力をそのまま使います)" ::
  " find [options|quantifier...]" ::
  "           po逆引きを行わないモード" ::
  " exit      終わる" ::
  "" ::
  "" ::
  "OPTIONS" ::
  " up to <number>  検索結果の最大表示数を変更する デフォルトでは25" ::
  " all             検索結果を全て表示する (up to 指定を無視する)" ::
  " show <key>      見つけたjsonオブジェクトの<key>の値のみを表示する" ::
  " translate       jsonオブジェクト内の文字列を翻訳して出力する" ::
  " disp path       そのjsonオブジェクトがどのfileにあったかを表示する" ::
  " short           コンパクト表示を行う" ::
  " num             検索結果の数を表示する" ::
  "" ::
  "QUANTIFIER" ::
  " <string> は文字列として扱う ([1,2,3]とかあっても配列じゃなくて文字列)" ::
  " 完全一致検索 " ::
  "  <key>=<string>  特定のキーと値の組を持つ" ::
  "  <key>=          特定のキーを持つ" ::
  "  =<string>       特定の値を持つ" ::
  " 部分一致検索 " ::
  "  <key>=?<string> 特定のキーを持ち、値に部分一致する組を持つ" ::  
  "  =?<string>      部分一致する値を持つ" ::
//  " name            poファイル内の名前っぽいもののみを対象にする" ::
//  " item            検索対象をアイテムに限定する (volume=)と等価" ::
  " no <quantifier> 直後の限定子を否定する" ::
  Nil

}

