package net.mauhiz.poustache

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import net.mauhiz.poustache.MustacheBlock.MustacheBlocks
import org.json4s.JsonAST._
import org.json4s.native.JsonMethods
import org.scalatest.{FunSpec, Matchers}

class MustacheOfficialSpec extends FunSpec with Matchers {

  for {
    specFile <- Seq("comments", "delimiters", "interpolation", "inverted", "partials", "sections")
  } {
    describe(s"Spec file: $specFile") {
      val specPath = Paths.get("src", "test", "resources", s"$specFile.json").toAbsolutePath
      val json = JsonMethods.parse(Files.newBufferedReader(specPath, StandardCharsets.UTF_8)).asInstanceOf[JObject].values
      val testCases = json("tests").asInstanceOf[Seq[Any]]

      for {
        testCase <- testCases
      } yield {
        val testObj = testCase.asInstanceOf[Map[String, Any]]
        val testName = testObj("name").asInstanceOf[String]
        val testTemplate = testObj("template").asInstanceOf[String]
        val testPartials = testObj.get("partials").map(_.asInstanceOf[Map[String, Any]]).getOrElse(Map.empty)
        val testData = testObj("data")
        it(s"should pass test: $testName ± whitespaces") {
          withClue(s"Data: $testData\nTemplate: $testTemplate\nPartials: $testPartials\n") {
            val (rendered, testExpected) = tryRender(specPath, testObj)
            rendered.diff(testExpected).trim shouldBe ""
            testExpected.diff(rendered).trim shouldBe ""
          }
        }

        ignore(s"should pass test: $testName") {
          withClue(s"Data: $testData\nTemplate: $testTemplate\n") {
            val (rendered, testExpected) = tryRender(specPath, testObj)
            rendered shouldBe testExpected
          }
        }
      }
    }
  }

  private def tryRender(specPath: Path, testObj: Map[String, Any]): (String, String) = {
    val testTemplate = testObj("template").asInstanceOf[String]
    val testPartials = testObj.get("partials").map(_.asInstanceOf[Map[String, Any]]).getOrElse(Map.empty)
    val testData = testObj("data")
    val testExpected = testObj("expected").asInstanceOf[String]
    val mustacheRoot = MustacheParser.parse(testTemplate)(lineSeparator(testTemplate))
    val context = MustacheContext(testData, strict = false)
    val rendered = mustacheRoot.render({ f: File => {
      testPartials.get(f.getName.stripSuffix(".mustache")) match {
        case Some(fileContents: String) => MustacheParser.parse(fileContents)(lineSeparator(fileContents))
        case _ => MustacheBlocks()
      }
    }
    }, specPath.toFile, context)
    (rendered, testExpected)
  }

  private def lineSeparator(s: String): LineSeparator = LineSeparator {
    (s.contains('\r'), s.contains('\n')) match {
      case (true, false) => "\r"
      case (true, true) => "\r\n"
      case _ => "\n"
    }
  }
}
