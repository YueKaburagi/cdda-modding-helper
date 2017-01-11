
package cddamod

import org.json4s._

import java.io.File

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

object BrowserLoader extends Loader {
  def loadBrowser(dir: File): Browser = {
    new Browser(recursiveLoad(dir))
  }
}

