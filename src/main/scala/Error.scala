
package cddamod

import org.json4s.JValue
import org.json4s.native.Printer.pretty

sealed trait Error
case class KeyNotFound(key: String, at: JValue) extends Error {
  override def toString = "KeyNotFound[\""+key+"\"]\n" + pretty(MorePretty prend at)
}
case class UnexpectedValueType(at: JValue) extends Error {
  override def toString = "UnexpectedValueType\n" + pretty(MorePretty prend at)
}
case class ExpectedValueType(str: String, actual: JValue) extends Error
case class UnmatchedValueType(required: String, actual: JValue) extends Error {
  override def toString = 
    "UnexpectedValueType\n require:"+ required + "\n actual:"+ pretty(MorePretty prend actual)
}
case class ParseError(failureString: String, atWord: String) extends Error


package importerror {
trait ImportError extends Error
object NoBrowser extends ImportError
object RefNotFound extends ImportError
object MultiRef extends ImportError
object NoBindIdent extends ImportError
object Reserved extends ImportError
case class FormatError(opt: String) extends ImportError
case class NotFoundIdent(ident: String) extends ImportError
case class NoSupport(code: String) extends ImportError
}


package prompterror {
trait PromptError extends Error
object NoDictionary extends PromptError { override def toString = "NoDictionary" }
object NoDictionaryOrder extends PromptError { override def toString = "NoDictionaryOrder" }
object NoBrowser extends PromptError { override def toString = "NoBrowser" }
object InvalidQueryFormat extends PromptError {override def toString = "InvalidQueryFormat" }
object NoSuchCommand extends PromptError { override def toString = "NoSuchCommand" }
}
