package net.mauhiz.poustache

import org.parboiled2._
import MustacheModel._

import scala.language.implicitConversions

class MustacheParser(val input: ParserInput) extends Parser with StringBuilding {
import MustacheParser._

  private def WS: Rule0 = rule {
    zeroOrMore(whitespace)
  }

  private def mbCrLf: Rule0 = rule {
    optional('\r') ~ optional('\n')
  }

  private def closeNl: Rule0 = rule {
    close ~ mbCrLf
  }

  private def contextName: Rule1[String] = rule {
    WS ~ clearSB() ~ oneOrMore(validContextName ~ appendSB()) ~ push(sb.toString) ~ WS
  }

  private def contents: Rule1[ContentsBlock] = rule {
    clearSB() ~ oneOrMore(!open ~ validContents ~ appendSB()) ~ push(sb.toString) ~> ContentsBlock
  }

  private def partialName: Rule1[String] = rule {
    WS ~ clearSB() ~ oneOrMore(validPartialName ~ appendSB()) ~ push(sb.toString) ~ WS
  }

  /**
   * Comment tags represent content that should never appear in the resulting output.
   * The tag's content may contain any substring (including newlines) EXCEPT the closing delimiter.
   * Comment tags SHOULD be treated as standalone when appropriate.
   */
  private def comment: Rule1[MustacheBlock] = rule {
    open ~ str("!") ~ oneOrMore(!close ~ CharPredicate.All)~ closeNl ~ push(CommentBlock)
  }

  private def partial: Rule1[PartialBlock] = rule {
    open ~ str(">") ~ push(cursor) ~ partialName ~> PartialBlock ~ closeNl
  }

  private def enterContext: Rule1[String] = rule {
    open ~ str("#") ~ contextName ~ closeNl
  }

  private def exitContext: Rule1[String] = rule {
    open ~ str("/") ~ contextName ~ closeNl
  }

  // TODO do not ignore delimiter change
  /**
   *  Set Delimiter tags are used to change the tag delimiters for all content following the tag in the current compilation unit.
   *  The tag's content MUST be any two non-whitespace sequences (separated by whitespace) EXCEPT an equals sign ('=') followed by the current closing delimiter.
   *  Set Delimiter tags SHOULD be treated as standalone when appropriate.
   */
  private def delimiterChange: Rule1[MustacheBlock] = rule {
    open ~ str("=") ~ WS ~ oneOrMore(validTag) ~ oneOrMore(whitespace) ~ oneOrMore(validTag) ~ WS ~ str("=") ~ closeNl ~ push(CommentBlock)
  }

  private def inverseContext: Rule1[String] = rule {
    open ~ str("^") ~ contextName ~ closeNl
  }

  private def inContext: Rule1[ContextBlock] = rule {
    (enterContext ~ push(cursor) ~ blocks ~ exitContext) ~> makeContextBlock
  }
  private def inInverseContext: Rule1[InverseContextBlock] = rule {
    (inverseContext ~ push(cursor) ~ blocks ~ exitContext) ~> makeInverseContextBlock
  }

  private def variable: Rule1[MustacheBlock] = rule {
    open ~ WS ~ push(cursor) ~ contextName ~> VariableBlock ~ close
  }

  private def variableNoEscape: Rule1[VariableNoEscapeBlock] = rule {
    (open ~ ((str("{") ~ WS ~ push(cursor) ~ contextName ~ str("}")) | (str("&") ~ WS ~ push(cursor)  ~ contextName)) ~ close) ~> VariableNoEscapeBlock
  }

  private def block: Rule1[MustacheBlock] = rule {
    inContext | inInverseContext | partial | contents | comment | variable | variableNoEscape | delimiterChange
  }

  private def blocks: Rule1[Seq[MustacheBlock]] = rule {
    zeroOrMore(block)
  }

  def mustache: Rule1[MustacheRoot] = rule {
    blocks ~> makeMustacheRoot ~ EOI
  }

}

object MustacheParser {

  private val validTag = CharPredicate.Visible -- '='
  private val validContextName = CharPredicate.Visible -- "!>#/^<{}&=\\"
  private val validPartialName = CharPredicate.Visible -- "!>#^<{}&=\\"
  private val validContents = CharPredicate.All -- EOI
  private val whitespace = CharPredicate.from(Character.isWhitespace)
  private val open = "{{"
  private val close = "}}"
  private val makeMustacheRoot: (Seq[MustacheBlock]) => MustacheRoot = blocks => MustacheRoot(blocks: _*)

  private val makeInverseContextBlock: (String, Int, Seq[MustacheBlock], String) => InverseContextBlock = (inC: String, pos: Int, b: Seq[MustacheBlock], outC: String) => {
    require(inC == outC)
    InverseContextBlock(pos, inC, b: _*)
  }

  private val makeContextBlock: (String, Int, Seq[MustacheBlock], String) => ContextBlock = (inC: String, pos: Int, b: Seq[MustacheBlock], outC: String) => {
    require(inC == outC)
    ContextBlock(pos, inC, b: _*)
  }

}