
package cddamod

import scalaz._
import scalaz.Scalaz._

import org.json4s._
import org.json4s.native.Document // instead scala.text.Document


// json4sのrenderに介入してrender ruleを一部変更する方法が思い付かなかったので
//  org.json4s.nafive.JsonMethods をコピペって改悪 ぐぬぬ
object MorePretty extends UT {
  import Document._
  var indent = 4

  def prend(jv: JValue): Document =
    specializeComponent(jv) match {
      case \/-(d) => d
      case -\/(v) => 
	v match {
	  case null          => text("null")
	  case JBool(true)   => text("true")
	  case JBool(false)  => text("false")
	  case JDouble(n)    => text(n.toString)
	  case JDecimal(n)   => text(n.toString)
	  case JLong(n)      => text(n.toString)
	  case JInt(n)       => text(n.toString)
	  case JNull         => text("null")
	  case JNothing      => sys.error("can't render 'nothing'")
	  case JString(null) => text("null")
	  case JString(s)    => text("\""+ParserUtil.quote(s)+"\"")
	  case JArray(arr)   => renderArr(arr)
	  case JSet(set)     => renderArr(set)
	  case JObject(obj)  =>
            val nested = break :: fields(trimObj(obj).map{
	      fld =>
		specializeRows(fld) match {
		  case \/-(r) => r
		  case -\/(l) => renderField(l)
		}})
            text("{") :: nest(indent, nested) :: break :: text("}")
	}
    }

  private def trimArr(xs: Iterable[JValue]) = xs.withFilter(_ != JNothing)
  private def trimObj(xs: List[JField]) = xs.filter(_._2 != JNothing)
  private def series(docs: Iterable[Document]) = punctuate(text(","), docs)
  private def fields(docs: List[Document]) = punctuate(text(",") :: break, docs)

  private def punctuate(p: Document, docs: Iterable[Document]): Document =
    if (docs.isEmpty) empty
    else docs.reduceLeft((d1, d2) => d1 :: p :: d2)

  private def renderArr(arr: Iterable[JValue]): Document =
    text("[") :: series(trimArr(arr).map(prend)) :: text("]")
  private def renderField(fld: JField): Document =
    fld match {
      case (n,v) => text("\""+ParserUtil.quote(n)+"\": ") :: prend(v)
    }

  // for [[[]]] like component
  private def specializeComponent(jv: JValue): (JValue \/ Document) =
    jv match {
      case JArray(vs0) => mapE(vs0) {
	case JArray(vs1) => mapE(vs1) {
	  case JArray(vs@ (JString(_) :: JInt(_) :: Nil)) =>
	    renderArr(vs).right
	  case _ => jv.left
	} map fields map {nest(indent,_)} map {
	  d => text("["+ (" "*(indent -1))) :: d :: break :: text("]")
	}
	case _ => jv.left
      } map fields map {d => nest(indent, break :: d)} map {d => text("[") :: d :: break :: text("]")}
      case _  => jv.left
    }

  // for terrain row
  private def specializeRows(fld: JField): (JField \/ Document) =
    fld match {
      case ("rows", JArray(vs)) => {text("\"rows\": [") :: 
	nest(indent, break :: fields{vs map prend}) :: break :: text("]")}.right
      case _ => fld.left
    }
}

