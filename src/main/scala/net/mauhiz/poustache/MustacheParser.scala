package net.mauhiz.poustache

import org.parboiled2._
import MustacheModel._

import scala.language.implicitConversions

class MustacheParser(val input: ParserInput) extends Parser with StringBuilding {
import MustacheParser._

  private def WS: Rule0 = rule {
    zeroOrMore(whitespace)
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

  private def comment: Rule1[MustacheBlock] = rule {
    open ~ str("!") ~ oneOrMore(!close ~ CharPredicate.All)~ close ~ push(CommentBlock)
  }

  private def partial: Rule1[PartialBlock] = rule {
    open ~ str(">") ~ push(cursor) ~ partialName ~> PartialBlock ~ close
  }

  private def enterContext: Rule1[String] = rule {
    open ~ str("#") ~ contextName ~ close
  }

  private def exitContext: Rule1[String] = rule {
    open ~ str("/") ~ contextName ~ close
  }

  private def inverseContext: Rule1[String] = rule {
    open ~ str("^") ~ contextName ~ close
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
    (open ~ ((str("{") ~ WS ~ push(cursor) ~ contextName ~ str("}")) | (str("&") ~ push(cursor) ~ WS ~ contextName)) ~ close) ~> VariableNoEscapeBlock
  }

  private def block: Rule1[MustacheBlock] = rule {
    inContext | inInverseContext | partial | contents | comment | variable | variableNoEscape
  }

  private def blocks: Rule1[Seq[MustacheBlock]] = rule {
    zeroOrMore(block)
  }

  def mustache: Rule1[MustacheRoot] = rule {
    blocks ~> makeMustacheRoot ~ EOI
  }

}

object MustacheParser {

  private val validContextName = CharPredicate.Visible -- "!>#/^<{}\\"
  private val validPartialName = CharPredicate.Visible -- "!>#^<{}\\"
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