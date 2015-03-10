package net.mauhiz.poustache

import scala.util.matching.Regex

case class Delimiter(open: String, close: String)

object Delimiter {
  val Default = Delimiter("{{", "}}")
  private val delimiterRegex: Regex = "\\s*([^\\s=]+)\\s*([^\\s=]+)\\s*".r

  def fromStr(startPos: Int, delimiterStr: String): Delimiter = {
    delimiterStr match {
      case delimiterRegex(open, close) => Delimiter(open, close)
      case _ => throw new TemplateParseException(startPos, s"Invalid delimiter: $delimiterStr")
    }
  }
}
