package ai.lum.regextools

import fastparse._
import NoWhitespace._
import RegexUtils.metacharacters
import RegexBuilder._

object RegexParser {

  def parseRegex(s: String) = parse(s, alternation(_))

  def alternation[_: P]: P[Pattern] = {
    P(concatenation.rep(sep="|")).map {
      case Seq(pattern) => pattern
      case patterns => Alternation(patterns.toList)
    }
  }

  def concatenation[_: P]: P[Pattern] = {
    P(quantified.rep).map {
      case Seq(pattern) => pattern
      case patterns => Concatenation(patterns.toList)
    }
  }

  def quantified[_: P]: P[Pattern] = {
    P(atomic ~ operator.?).map {
      case (pattern, None) => pattern
      case (pattern, Some(Quantifier(0, None))) => KleeneStar(pattern)
      case (pattern, Some(Quantifier(0, Some(1)))) => Optional(pattern)
      case (pattern@_, Some(Quantifier(1, None))) => ???
      case (pattern@_, Some(Quantifier(min@_, None))) => ???
      case (pattern@_, Some(Quantifier(min@_, Some(max@_)))) => ???
    }
  }

  case class Quantifier(min: Int, max: Option[Int])

  def operator[_: P]: P[Quantifier] = {
    P(quantifier | range | repetition)
  }

  def quantifier[_: P]: P[Quantifier] = {
    P(CharIn("*+?").!).map {
      case "*" => Quantifier(0, None)
      case "+" => Quantifier(1, None)
      case "?" => Quantifier(0, Some(1))
    }
  }

  def range[_: P]: P[Quantifier] = {
    P("{" ~ number.? ~ "," ~ number.? ~ "}").map {
      case (None, max) => Quantifier(0, max)
      case (Some(min), max) => Quantifier(min, max)
    }
  }

  def repetition[_: P]: P[Quantifier] = {
    P("{" ~ number ~ "}").map {
      case n => Quantifier(n, Some(n))
    }
  }

  def atomic[_: P]: P[Pattern] = {
    P(symbol | "(" ~ alternation ~ ")")
  }

  def symbol[_: P]: P[Symbol] = {
    P(CharPred(c => !metacharacters.contains(c)).! | "\\" ~ AnyChar.!).map(Symbol)
  }

  def number[_: P]: P[Int] = {
    P(CharsWhile(_.isDigit).!.map(_.toInt))
  }

}
