package ai.lum.regextools

import fastparse._
import NoWhitespace._
import RegexUtils.isRegexMetaCharacter
import RegexAst._

object RegexParser {

  def parseRegex(s: String) = parse(s, alternation(_))

  def alternation[_: P]: P[Pattern] = {
    P(concatenation.rep(sep="|")).map {
      case Seq(pattern) => pattern
      case patterns =>
        Alternation(patterns.toList.flatMap {
          case Alternation(clauses) => clauses
          case p => List(p)
        })
    }
  }

  def concatenation[_: P]: P[Pattern] = {
    P(quantified.rep).map {
      case Seq(pattern) => pattern
      case patterns =>
        Concatenation(patterns.toList.flatMap {
          case Concatenation(clauses) => clauses
          case p => List(p)
        })
    }
  }

  def quantified[_: P]: P[Pattern] = {
    P(atomic ~ operator.?).map {
      case (pattern, None) => pattern
      case (pattern, Some(Quantifier(0, None))) => KleeneStar(pattern)
      case (pattern, Some(Quantifier(0, Some(1)))) => Optional(pattern)
      case (pattern, Some(Quantifier(1, Some(1)))) => pattern
      case (pattern, Some(Quantifier(min, None))) =>
        val required = List.fill(min)(pattern)
        Concatenation(required :+ KleeneStar(pattern))
      case (pattern, Some(Quantifier(min, Some(max)))) =>
        val required = List.fill(min)(pattern)
        val optional = List.fill(max - min)(Optional(pattern))
        Concatenation(required ++ optional)
    }
  }

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
    P(symbol | escape | "(" ~ alternation ~ ")")
  }

  def symbol[_: P]: P[Symbol] = {
    P(CharPred(c => !isRegexMetaCharacter(c)).!.map(Symbol))
  }

  def escape[_: P]: P[Pattern] = {
    P("\\" ~ AnyChar.!).map {
      // TODO escape sequences like \w \d \s
      case c => Symbol(c)
    }
  }

  def number[_: P]: P[Int] = {
    P(CharsWhile(_.isDigit).!.map(_.toInt))
  }

}
