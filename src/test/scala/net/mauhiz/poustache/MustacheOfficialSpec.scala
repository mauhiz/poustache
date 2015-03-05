package net.mauhiz.poustache

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import org.json4s.JsonAST._
import org.json4s.native.JsonMethods
import org.parboiled2.ParseError
import org.scalatest.{FunSpec, Matchers}

import scala.util.control.NonFatal

class MustacheOfficialSpec extends FunSpec with Matchers {

  import org.parboiled2.Parser.DeliveryScheme.Throw

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
        it(s"should pass test: $testName") {
          val testTemplate = testObj("template").asInstanceOf[String]
          val testData = testObj("data")
          withClue(s"Data: $testData\nTemplate: $testTemplate") {
            try {
            val testExpected = testObj("expected").asInstanceOf[String]
              val parser = new MustacheParser(testTemplate)
              val mustacheRoot = parser.mustache.run()
              val context = MustacheContext(testData, strict = false)
              def currentFile: File = new File(".")
              mustacheRoot.render({ f: File => ???}, currentFile, context) shouldBe testExpected
            } catch {
              case pe: ParseError => fail(s"Could not parse: $testTemplate")
              case nie: NotImplementedError => fail("Oops")
              case NonFatal(e) => fail(e)
            }
          }
        }
      }
    }
  }
}
