package net.mauhiz.poustache

import org.scalatest.{Matchers, FunSpec}

class MustacheContextSpec extends FunSpec with Matchers {

  describe("#escapeSingleChar") {
    it("should escape XML special characters") {
      MustacheContext.escapeSingleChar('>') shouldBe Some("&gt;")
    }
  }
}
