
package cddamod

import scala.language.postfixOps

import org.json4s._

import java.io.File

case class JInfo(jv: JValue, file: File)

class Browser(jinfos: Set[JInfo]) extends UT {

  private[this] def jvs: Set[JValue] = jinfos map (_.jv)

  def lookupXsf(fs: Seq[JObjectFilter]): Set[JInfo] =
    jinfos filter {v => fs forall {contains(_)(v.jv)}}

  def lookupXs(fs: Seq[JObjectFilter]): Set[JValue] =
    jvs filter {v => fs forall {contains(_)(v)}}

  def lookup(f: JObjectFilter): Set[JValue] =
    jvs filter {contains(f)}
  
  def contains(f: JObjectFilter)(jv: JValue): Boolean =
    jv match {
      case JObject(fs) => f apply fs
      case _ => false
    }
}

object BrowserLoader extends Loader {
  def loadBrowser(dir: File): Browser = {
    new Browser(listAllJValuesWithFile(dir) map {case (jv,f) => JInfo(jv,f)} toSet)
  }
}

