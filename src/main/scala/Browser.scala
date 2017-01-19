
package cddamod

import scala.language.postfixOps

import org.json4s._

import java.io.File

case class JInfo(jv: JValue, file: File, index: Browser#Index)

class Browser(jinfos: Map[Browser#Index, JInfo]) extends UT {
  type Index = Int

  private[this] def jvs: Set[JValue] = jinfos.values.toSet map {ji: JInfo => ji.jv}

  def lookupById(str: String): Option[(Index, JInfo)] = {
    def i = str.toInt
    jinfos get i map {(i,_)}
  }

  def lookupXsf(fs: Set[JItemFilter]): Map[Index, JInfo] = {
    val (ids, ffs) = fs partition {
      case ById(_) => true
      case _ => false
    }
    def src = if (ids nonEmpty) {
      ids flatMap {
        case ById(s) => lookupById(s)
        case _ => None
      } toMap
    } else {jinfos}
    src filter {case (k,v) => ffs forall {_ apply (k,v)} }
  }
    
  def lookupXs(fs: Seq[JObjectFilter]): Set[JValue] =
    jvs filter {v => fs forall {
      f =>
      v match {
        case JObject(fs) => f withJObject fs
        case _ => false
      }
    }}

}

object BrowserLoader extends Loader {
  def loadBrowser(dir: File): Browser = {
    new Browser(listAllJValuesWithFile(dir) map {
      case (jv,f) =>
        // このあたりで jo でない jvalue を検出してみるといいかと
        val t = nextId()
        (t, JInfo(jv,f,t))
    } toMap)
  }

  // 一意に決定できればいいだけなので
  private[this] var count: Int = 0
  def nextId(): Int = {
    count += 1
    count
  }
}

