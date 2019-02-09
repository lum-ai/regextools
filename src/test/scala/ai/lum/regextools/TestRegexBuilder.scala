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

  it should "use the optional operator" in {
    val builder = new RegexBuilder
    builder.add("foobar")
    builder.mkPattern shouldEqual "foobar"
    builder.add("fooar")
    builder.mkPattern shouldEqual "foob?ar"
  }

  it should "build pattern from urls" in {
    val builder = new RegexBuilder
    builder.add("appserver1.domain.tld")
    builder.mkPattern shouldEqual "appserver1\\.domain\\.tld"
    builder.add("appserver2.domain.tld")
    builder.mkPattern shouldEqual "appserver[12]\\.domain\\.tld"
    builder.add("appserver3.domain.tld")
    builder.mkPattern shouldEqual "appserver[123]\\.domain\\.tld"
  }

}
