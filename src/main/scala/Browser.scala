
package cddamod

import scala.language.postfixOps

import scalaz.Scalaz._

import org.json4s._
import org.json4s.JsonDSL._

import java.io.File

case class JInfo(jv: JValue, file: File, index: Browser#Index, raw: JValue)
case class ModInfo(root: File, ident: String, name: String, ji: JInfo)

object Browser {
  def isCDDA(mi: => ModInfo): Boolean =
    mi.ident == "dda"

  def whereis(mis: => Set[ModInfo])(target: => JInfo): Option[ModInfo] =
    mis find {case ModInfo(rt,_,_,_) => target.file.getCanonicalPath startsWith rt.getCanonicalPath}

  def modContains(mi: => ModInfo)(ji: => JInfo): Boolean =
    ji.file.getCanonicalPath startsWith mi.root.getCanonicalPath
}

trait Browser extends UT {
  type Index = Int

  val infos: Map[Index, JInfo]
  val mods: Set[ModInfo] = Set.empty
  // cache

  def modsAsSource: Map[Index, JInfo] =
    mods map {
      case ModInfo(f,ident,name,JInfo(_,_,ix,r)) =>
        val jv: JObject = {("ident" -> ident) ~ ("name" -> name)}
        (ix, JInfo(jv,f,ix,r))
    } toMap
  def indexesAsSource(ids: => Set[String]): Map[Index, JInfo] =
    ids flatMap {this lookupById _} toMap
  def source: Map[Index, JInfo] = infos


  private[this] def jvs: Set[JValue] = infos.values.toSet map {ji: JInfo => ji.jv}

  val whereis = Browser.whereis(mods) _

  def lookupByIdent(ident: => String): Option[ModInfo] =
    mods find (_.ident == ident)

  def lookupById(str: => String): Option[(Index, JInfo)] = {
    def i = str.toInt
    infos get i map {(i,_)}
  }

  def lookupXsf(fs: => Set[JItemFilter]): Map[Index, JInfo] = {
    val (ids, ffs) = fs partition {
      case ById(_) => true
      case _ => false
    }
    def src = if (ids nonEmpty) {
      ids flatMap {
        case ById(s) => lookupById(s)
        case _ => None
      } toMap
    } else {infos}
    src filter {case (k,v) => ffs forall {_ apply (k,v)} }
  }
    
  def lookupXs(fs: => Seq[JObjectFilter]): Set[JValue] =
    jvs filter {v => fs forall {
      f =>
      v match {
        case JObject(fs) => f withJObject fs
        case _ => false
      }
    }}


}


object BrowserLoader extends Loader with DoAny {
  def loadBrowser(dir: File): Browser = {
    // ちょっと大きいので par にするべき？
    // file io が遅いのはしょうがない？
    val jz = listAllJValuesWithFile(dir)
    val jinfos = jz map {
      case (jv,f) =>
        // このあたりで jo でない jvalue を検出してみるといいかと
        val t = nextId()
        (t, JInfo(jv,f,t,jv))
    }
    val modinfos = jinfos map second flatMap {
      case ji@JInfo(jv,f,_,_) =>
        jv lookup "type" exists (_ == JString("MOD_INFO")) match {
          case false => None
          case true =>
            for {
              root <- jv lookup "path" flatMap {
                case JString(s) => s.some
                case _ => None
              } orElse {".".some} flatMap {s => Option(f getParentFile) map {new File(_, s)}}
              ident <- jv lookup "ident" flatMap {case JString(s) => s.some; case _ => None}
              name <- jv lookup "name" flatMap {case JString(s) => s.some; case _ => None}
            } yield ( ModInfo(root,ident,name,ji) )
        }
    } toSet
    // 検索高速化のためのインデックス作成 (copy-from用)
    val lu: Map[JString, JValue] = {jz flatMap {
      case (jv,_) =>
        (jv lookup "id") orElse (jv lookup "abstract") flatMap {
          case js@JString(_) =>
            // rebalance系mod による既存アイテムの override 形式のjsonでひっかかる循環の回避
            // 誰かが相互置換みたいなことやってると将来バグる (二次以上の循環は検出できない)
            // jsonがどのmodにあったかで突破できない？
            // 複数あったら Browser.isCDDA(Browser.whereis(modinfos)(jv)) にかける？
            if (jv lookup "copy-from" exists (_ == js)) {None} else (js,jv).some
          case _ => None
        }
    } toMap}
    new Browser {
      override val infos = jinfos map wrap(solveCopyFrom(lu.get)) toMap
      override val mods = modinfos
    }
  }
  private[this] def wrap(f: JValue => JValue)(ji: (Browser#Index, JInfo)): (Browser#Index, JInfo) =
    ji match {
      case (i,JInfo(jv,a,b,c)) => (i, JInfo(f(jv),a,b,c))
    }
  val mergeIgnores = "name_plural" :: "abstract" :: Nil
  private[this] def merge(c: => JValue)(p: JValue): JValue =
    p match {
      case JObject(pfs) =>
        c match {
          case JObject(cfs) =>
            JObject(
              {pfs flatMap {
                case (k,_) if mergeIgnores contains k => None
                case kv@(k,_) =>
                  if (cfs exists {case (i,_) => i == k}) {None} else {kv.some}
              }} |+| cfs
            )
          case _ => c
        }
      case _ => c
    }
  // あと using も解決したい
  private[this] def solveCopyFrom(f: JString => Option[JValue])(jv: JValue): JValue = 
    jv lookup "copy-from" match {
      case None => jv
      case Some(js@JString(_)) => f(js) map {
        v => merge( jv )( solveCopyFrom(f)(v) )
      } getOrElse jv
      case Some(_) => jv
    }


  // 一意に決定できればいいだけなので
  private[this] var count: Int = 0
  def nextId(): Int = {
    count += 1
    count
  }
}

