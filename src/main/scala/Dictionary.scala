
package cddamod

import scala.language.postfixOps

import org.json4s._

import java.io.File

import scala.io.Source
import scala.collection.mutable
import scala.io.Codec


trait DictionaryElement {
  val str: String
}
case class AsText(override val str: String) extends DictionaryElement
case class AsName(override val str: String) extends DictionaryElement
case class AsString(override val str: String) extends DictionaryElement
object Blank extends DictionaryElement {override val str = "()"}

object DictLoader {

  def load(pofile: File): Dictionary = {
    val src = Source.fromFile(pofile)(Codec.UTF8).getLines
    new Dictionary( pLine(src, Ready, mutable.Map.empty).toMap )
  }
  private[this] def pLine(lines: Iterator[String], mode: Mode,
			  m: mutable.Map[DictionaryElement,List[String]]):
  mutable.Map[DictionaryElement,List[String]] = {
    mode match {
      case Ready =>
	if (lines.hasNext) {
	  lines.next match {
	    case msgid(s) => pLine(lines, Index(s), m)
	    case _ => pLine(lines, Ready, m)
	  }
	} else { m }
      case Index(id) =>
	if (lines.hasNext) {
	  lines.next match {
	    case msgstr(s) => pLine(lines, Pair(id,AsString(s)), m)
	    case msgstr0(s) => pLine(lines, Pair(id,AsName(s)), m)
	    case msgplural(_) => pLine(lines, Index(id), m) // 日本語じゃ必要ないので読み捨て
            case stringline(s) => pLine(lines, Index(id + s), m)
	    case _ => pLine(lines, Ready, m)
	  }
	} else { Log.error("invalid po file"); m }
      case Pair(id,elem) =>
	if (lines.hasNext) {
	  lines.next match {
	    case stringline(s) => pLine(lines, Pair(id,AsText(elem.str + s)), m)
	    case _ => pLine(lines, Ready, upsert(m,id,elem))
	  }
	} else { upsert(m,id,elem) }
    }
  }
  private[this] trait Mode
  private[this] object Ready extends Mode
  private[this] case class Index(id: String) extends Mode
  private[this] case class Pair(id: String, e: DictionaryElement) extends Mode
  
  private[this] val msgid = """msgid\s+"(.*)"""".r
  private[this] val msgplural = """msgid_plural\s+"(.*)"""".r
  private[this] val msgstr = """msgstr\s+"(.*)"""".r
  private[this] val msgstr0 = """msgstr\[\d+\]\s+"(.*)"""".r
  private[this] val stringline = """"(.*)"""".r
  private[this] def upsert(m: mutable.Map[DictionaryElement,List[String]],
	     id: String, elem: DictionaryElement): mutable.Map[DictionaryElement,List[String]] = {
    m get elem match {
      case Some(v) => m.update(elem, id :: v)
      case None => m.update(elem, id :: Nil)
    }
    m
  }

}

class Dictionary(src: Map[DictionaryElement, List[String]]) {
  // 逆向きのキャッシュをつくって高速化を図りたい

  def lookup(str: String): List[(DictionaryElement, String)] = 
  {src filter {case (k,v) => k.str contains str} toList} flatMap {case (k,vs) => vs map {(k,_)}}

  def nameLookup(str: String): List[(DictionaryElement, String)] = 
    {src filter {
      case (AsName(k),v) => k contains str
      case _ => false
    } toList} flatMap {case (k,vs) => vs map {(k,_)}}

  def lookupS(str: String): Set[String] =
    src.filter{case (k,v) => k.str containsSlice str}.values.flatten.toSet
  def findS(str: String): Set[String] =
    src.filter{case (k,v) => k.str == str}.values.flatten.toSet

  def rfind(str: String): Option[String] =
    src.filter{case (k,v) => v contains str}.keys.map{_.str}.headOption

  def translate(jv: JValue): JValue =
    jv match {
      case JObject(fs) => JObject( fs map {
        case kv@(k,v) =>
          if (Configuration translationTargets k) {
            (k, v match {
              case JString(s) => JString(this rfind s getOrElse s)
              case jo@JObject(_) => translate(jo)
              case x => x
            })
          } else { kv }
      })
      case x => x
    }
}

