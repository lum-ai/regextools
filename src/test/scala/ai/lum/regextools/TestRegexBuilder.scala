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
    builder.mkPattern shouldEqual "foo([bx]ar|zap)"
  }

  it should "use the optional operator correctly 1" in {
    val builder = new RegexBuilder
    builder.add("foobar")
    builder.mkPattern shouldEqual "foobar"
    builder.add("fooar")
    builder.mkPattern shouldEqual "foob?ar"
  }

  it should "use the optional operator correctly 2" in {
    val builder = new RegexBuilder
    builder.add("a")
    builder.mkPattern shouldEqual "a"
    builder.add("aa")
    builder.mkPattern shouldEqual "aa?"
    builder.add("aaa")
    builder.mkPattern shouldEqual "aa?a?"
    builder.add("aaaa")
    builder.mkPattern shouldEqual "aa?a?a?"
    builder.add("aaaaa")
    builder.mkPattern shouldEqual "aa?a?a?a?"
    builder.add("aaaaaa")
    builder.mkPattern shouldEqual "aa?a?a?a?a?"
  }

  it should "use the optional operator correctly 3" in {
    val builder = new RegexBuilder
    builder.add("abc")
    builder.mkPattern shouldEqual "abc"
    builder.add("abcd")
    builder.mkPattern shouldEqual "abcd?"
    builder.add("abcde")
    builder.mkPattern shouldEqual "abc(de?)?"
    builder.add("abcdef")
    builder.mkPattern shouldEqual "abc(d(ef?)?)?"
    builder.add("abcef")
    builder.mkPattern shouldEqual "abc(d(ef?)?|ef)?"
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

  "OdinPatternBuilder" should "build pattern from syntax paths" in {
    val builder = new OdinPatternBuilder
    builder.add(">dobj")
    builder.mkPattern shouldEqual ">dobj"
    builder.add(">dobj >nmod_in")
    builder.mkPattern shouldEqual ">dobj >nmod_in?"
    builder.add("<nsubj >det")
    builder.mkPattern shouldEqual "<nsubj >det | >dobj >nmod_in?"
    builder.add(">dobj >nmod_in >det")
    builder.mkPattern shouldEqual "<nsubj >det | >dobj (>nmod_in >det?)?"
    builder.add(">dobj >det")
    builder.mkPattern shouldEqual "<nsubj >det | >dobj >nmod_in? >det?"
  }

}
