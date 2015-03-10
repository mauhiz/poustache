package net.mauhiz.poustache

import net.mauhiz.poustache.MustacheAST._
import org.scalatest.{FunSpec, Matchers}

class MustacheParserSpec extends FunSpec with Matchers {

  describe("flattenMultilineTags") {
    it("should flatten multi-line comments") {
      MustacheParser.flattenMultilineTags("{{!a\nb}}", Delimiter.Default)(LineSeparator.Unix) shouldBe "{{!a b}}"
    }
  }
  describe("parseWithDelim") {
    it("should return expected AST") {
      val template = "|\n{{#boolean}}\n{{/boolean}}\n|"
      MustacheParser.parseWithDelim(template, Delimiter.Default)(LineSeparator.Unix) shouldBe Seq(
        Contents(0, "|\n", 2),
        EnterContext(2, "boolean", 14),
        Contents(14, "\n", 15),
        ExitContext(15, "boolean", 27),
        Contents(27, "\n|", 29)
      )
    }
  }

  describe("splitByDelim") {
    it("should find the new delim") {
      val template = "{{=<% %>=}}(<%text%>)"
      MustacheParser.splitByDelim(template) shouldBe Seq(
        "{{=<% %>=}}" -> Delimiter.Default,
        "(<%text%>)" -> Delimiter("<%", "%>")
      )
    }
  }

  describe("handleStandalone") {
    it("should trim in the middle of a template") {
      val template = "|\n {{!cmt}} \n|"
      MustacheParser.handleStandalone(template, Delimiter.Default)(LineSeparator.Unix) shouldBe "|\n{{!cmt}}|"
    }
    it("should trim in the beginning of a template") {
      val template = " {{!cmt}} \n|"
      MustacheParser.handleStandalone(template, Delimiter.Default)(LineSeparator.Unix) shouldBe "{{!cmt}}|"
    }
    it("should trim in the end of a template") {
      val template = "|\n {{!cmt}} "
      MustacheParser.handleStandalone(template, Delimiter.Default)(LineSeparator.Unix) shouldBe "|{{!cmt}}"
    }
  }
}
