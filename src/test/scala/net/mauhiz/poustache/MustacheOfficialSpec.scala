package net.mauhiz.poustache

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import net.mauhiz.poustache.MustacheBlock.MustacheBlocks
import org.json4s.JsonAST._
import org.json4s.native.JsonMethods
import org.scalatest.{FunSpec, Matchers}

import scala.util.Try

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
        val testData = testObj("data")
        it(s"should pass test: $testName ± whitespaces") {
          withClue(s"Data: $testData\nTemplate: $testTemplate\n") {
            val testExpected = testObj("expected").asInstanceOf[String]
            val mustacheRoot = MustacheParser.parse(testTemplate)(lineSeparator(testTemplate))
            val context = MustacheContext(testData, strict = false)
            mustacheRoot.render({ f: File => {
              Try {
                val fileContents = new String(Files.readAllBytes(f.toPath), StandardCharsets.UTF_8)
                MustacheParser.parse(fileContents)(lineSeparator(fileContents))
              } getOrElse MustacheBlocks()
            }
            }, specPath.toFile, context).diff(testExpected).trim shouldBe ""
          }
        }

        ignore(s"should pass test: $testName") {
          withClue(s"Data: $testData\nTemplate: $testTemplate\n") {
            val testExpected = testObj("expected").asInstanceOf[String]
            val mustacheRoot = MustacheParser.parse(testTemplate)(lineSeparator(testTemplate))
            val context = MustacheContext(testData, strict = false)
            mustacheRoot.render({ f: File => {
              Try {
                val fileContents = new String(Files.readAllBytes(f.toPath), StandardCharsets.UTF_8)
                MustacheParser.parse(fileContents)(lineSeparator(fileContents))
              } getOrElse MustacheBlocks()
            }
            }, specPath.toFile, context) shouldBe testExpected
          }
        }
      }
    }
  }

  private def lineSeparator(s: String): LineSeparator = LineSeparator {
    (s.contains('\r'), s.contains('\n')) match {
      case (true, false) => "\r"
      case (true, true) => "\r\n"
      case _ => "\n"
    }
  }
}
