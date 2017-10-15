package ai.lum.regextools

import org.scalatest.{ FlatSpec, Matchers }

class TestRegexBuilder extends FlatSpec with Matchers {

  "RegexBuilder" should "minimize regex correctly" in {
    val builder = new RegexBuilder
    builder.add("foobar")
    builder.mkPattern shouldEqual "foobar"
    builder.add("fooxar")
    builder.mkPattern shouldEqual "foo[bx]ar"
    builder.add("foozap")
    builder.mkPattern shouldEqual "foo(?:[bx]ar|zap)"
  }

}
