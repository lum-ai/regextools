/*
 * Copyright 2019 lum.ai
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ai.lum.regextools

class RegexBuilder(
    val useCharClass: Boolean = true,
    val separator: String = "",
    val openParens: String = "(",
    val closeParens: String = ")",
    val quote: String => String = RegexUtils.quote
) {

  import RegexBuilder._

  private var trie = new State

  def clear(): Unit = {
    trie = new State
  }

  /** tokenizes each string and adds the symbols to the trie */
  def add(strings: String*): Unit = {
    strings.foreach(s => addSymbols(s.split(separator)))
  }

  /** add a sequence of symbols to the trie */
  def addSymbols(symbols: Seq[String]): Unit = {
    var state = trie
    for (sym <- symbols) {
      state = state.transitions.getOrElseUpdate(sym, new State)
    }
    state.accepting = true
  }

  /** returns the minimized dfa in dot format */
  def mkDot: String = trie.minimize.mkDot

  /** returns a single pattern that matches all inputs */
  def mkPattern: String = {
    if (trie.transitions.isEmpty) {
      ""
    } else {
      stringify(dfaToPattern(trie.minimize))
    }
  }

  /** Returns a list of patterns that can be ORed together
   *  to match all inputs.
   */
  def mkPatterns: List[String] = {
    if (trie.transitions.isEmpty) {
      Nil
    } else {
      dfaToPattern(trie.minimize) match {
        // if the root of the AST is an Alternation
        // then return each of its branches individually
        case Alternation(patterns) => patterns.map(stringify)
        case pattern => List(stringify(pattern))
      }
    }
  }

  /** returns a pattern's string representation */
  private def stringify(p: Pattern): String = p match {
    case Epsilon           => "\u03B5" // GREEK SMALL LETTER EPSILON
    case Symbol(x)         => quote(x)
    case CharSet(xs)       => "[" + xs.map(x => quote(x.value)).sorted.mkString + "]"
    case Optional(x)       => stringifyWithParenthesis(x) + "?"
    case KleeneStar(x)     => stringifyWithParenthesis(x) + "*"
    case Alternation(xs)   => xs.map(stringify).mkString(s"$separator|$separator")
    case Concatenation(xs) => xs.map(stringifyWithParenthesis).mkString(separator)
  }

  /** convert pattern to string and surround it with parenthesis if needed */
  private def stringifyWithParenthesis(p: Pattern): String = p match {
    case a: Alternation   => s"$openParens${stringify(a)}$closeParens"
    case c: Concatenation => s"$openParens${stringify(c)}$closeParens"
    case _ => stringify(p)
  }

  // Brzozowski algebraic method
  private def dfaToPattern(root: State): Pattern = {
    val states = root.reachableStates
    val m = states.size

    // initialize equations
    val A = Array.ofDim[Pattern](m, m)
    val B = Array.ofDim[Pattern](m)
    for (i <- states.indices) {
      val s = states(i)
      if (s.accepting) {
        B(i) = Epsilon
      }
      for ((a, s2) <- s.transitions) {
        val j = states.indexOf(s2)
        A(i)(j) = union(A(i)(j), Symbol(a))
      }
    }

    // solve equations
    for (n <- m - 1 to 0 by -1) {
      if (A(n)(n) != null) {
        B(n) = concat(star(A(n)(n)), B(n))
        for (j <- 0 until n) {
          A(n)(j) = concat(star(A(n)(n)), A(n)(j))
        }
      }
      for (i <- 0 until n) {
        if (A(i)(n) != null) {
          B(i) = union(B(i), concat(A(i)(n), B(n)))
          for (j <- 0 until n) {
            A(i)(j) = union(A(i)(j), concat(A(i)(n), A(n)(j)))
          }
        }
      }
    }

    // return resulting pattern
    return B(0)

  }

  private def star(p: Pattern): Pattern = p match {
    case null => null
    case Epsilon => Epsilon
    case _ => KleeneStar(p)
  }

  private def concat(lhs: Pattern, rhs: Pattern): Pattern = {
    if (lhs == null || rhs == null) return null
    (lhs, rhs) match {
      case (a, Epsilon) => a
      case (Epsilon, b) => b
      case (Concatenation(as), Concatenation(bs)) => Concatenation(as ++ bs)
      case (a, Concatenation(bs)) => Concatenation(a +: bs)
      case (Concatenation(as), b) => Concatenation(as :+ b)
      case (a, b) => Concatenation(List(a, b))
    }
  }

  private def union(lhs: Pattern, rhs: Pattern): Pattern = {
    if (lhs == null) return rhs
    if (rhs == null) return lhs
    if (lhs == rhs) return lhs
    val (a, b, prefix, suffix) = commonPrefixSuffix(lhs, rhs)
    var result = (a, b) match {
      case (Concatenation(xs), Epsilon) => handleOptionalConcatenation(xs)
      case (Epsilon, Concatenation(xs)) => handleOptionalConcatenation(xs)
      case (a, Epsilon) => Optional(a)
      case (Epsilon, b) => Optional(b)
      case (Optional(a), Optional(b)) => Optional(alternation(List(a, b)))
      case (Optional(a), b) => Optional(alternation(List(a, b)))
      case (a, Optional(b)) => Optional(alternation(List(a, b)))
      case (Alternation(as), Alternation(bs)) => alternation(as ++ bs)
      case (a, Alternation(bs)) => alternation(a +: bs)
      case (Alternation(as), b) => alternation(as :+ b)
      case (a, b) => alternation(List(a, b))
    }
    // attach common prefix
    result = (prefix, result) match {
      case (Nil, r) => r
      case (px, Concatenation(rs)) => Concatenation(px ++ rs)
      case (px, r) => Concatenation(px :+ r)
    }
    // attach common suffix
    result = (result, suffix) match {
      case (r, Nil) => r
      case (Concatenation(rs), sx) => Concatenation(rs ++ sx)
      case (r, sx) => Concatenation(r +: sx)
    }
    result
  }

  // ensure patterns like `(xx?)?` are converted to `x?x?`
  private def handleOptionalConcatenation(xs: List[Pattern]): Pattern = {
    val last = xs.last
    if (last.isInstanceOf[Optional]) {
      // take all the optional components at the end of the concatenation
      val rxs = xs.reverse
      val optionals = rxs.takeWhile(_ == last).asInstanceOf[List[Optional]]
      // concatenate the rest of the components
      val required = concatenation(xs.dropRight(optionals.size))
      if (optionals.forall(_.pattern == required)) {
        // if all the optional patterns are equal to the required one
        // then return a sequence of optionals
        return Concatenation(Optional(required) :: optionals)
      }
    }
    // return the optional concatenation
    Optional(Concatenation(xs))
  }

  // collapses alternative chars into a CharClass
  private def alternation(xs: List[Pattern]): Pattern = {
    if (!useCharClass) return minimalAlternation(xs)
    val literals = xs.flatMap {
      case x: Symbol => Some(x)
      case _ => None
    }
    val charSets = xs.flatMap {
      case x: CharSet => Some(x)
      case _ => None
    }
    if (literals.size <= 1 && charSets.isEmpty) return Alternation(xs)
    if (xs.size == literals.size + charSets.size) {
      CharSet(literals ++ charSets.flatMap(_.chars))
    } else {
      val rest = xs.filterNot(x => literals.contains(x) || charSets.contains(x))
      Alternation(CharSet(literals ++ charSets.flatMap(_.chars)) +: rest)
    }
  }

  private def minimalAlternation(xs: List[Pattern]): Pattern = {
    var patterns = xs.distinct
    var newPatterns = Set(patterns.head)
    patterns = patterns.tail
    while (patterns.nonEmpty) {
      val p = patterns.head
      var addP = true
      patterns = patterns.tail
      for (np <- newPatterns) {
        val (p1, p2, prefix, suffix) = commonPrefixSuffix(p, np)
        if (prefix.nonEmpty || suffix.nonEmpty) {
          var result = (p1, p2) match {
            case (Concatenation(xs), Epsilon) => handleOptionalConcatenation(xs)
            case (Epsilon, Concatenation(xs)) => handleOptionalConcatenation(xs)
            case (a, Epsilon) => Optional(a)
            case (Epsilon, b) => Optional(b)
            case (Optional(a), Optional(b)) => Optional(alternation(List(a, b)))
            case (Optional(a), b) if a == b => Optional(a)
            case (a, Optional(b)) if a == b => Optional(b)
            case (Optional(a), b) => Optional(alternation(List(a, b)))
            case (a, Optional(b)) => Optional(alternation(List(a, b)))
            case (Alternation(as), Alternation(bs)) => alternation(as ++ bs)
            case (a, Alternation(bs)) => alternation(a +: bs)
            case (Alternation(as), b) => alternation(as :+ b)
            case (a, b) => alternation(List(a, b))
          }
          // attach common prefix
          result = (prefix, result) match {
            case (Nil, r) => r
            case (px, Concatenation(rs)) => Concatenation(px ++ rs)
            case (px, r) => Concatenation(px :+ r)
          }
          // attach common suffix
          result = (result, suffix) match {
            case (r, Nil) => r
            case (Concatenation(rs), sx) => Concatenation(rs ++ sx)
            case (r, sx) => Concatenation(r +: sx)
          }
          addP = false
          newPatterns -= np
          newPatterns += result
        }
      }
      if (addP) {
        newPatterns += p
      }
    }
    if (newPatterns.size == 1) {
      newPatterns.head
    } else {
      Alternation(newPatterns.toList)
    }
  }

  // returns the lhs and rhs with the common prefix and suffix removed
  // also returns the common prefix and suffix if there is any
  private def commonPrefixSuffix(
      lhs: Pattern,
      rhs: Pattern
  ): (Pattern, Pattern, List[Pattern], List[Pattern]) = {
    require(lhs != rhs, "lhs and rhs can't be equal")
    (lhs, rhs) match {
      case (Concatenation(as), Concatenation(bs)) =>
        val prefix = findCommonPrefix(as, bs)
        val suffix = findCommonSuffix(as, bs)
        val a = concatenation(as.slice(prefix.size, as.size - suffix.size))
        val b = concatenation(bs.slice(prefix.size, bs.size - suffix.size))
        (a, b, prefix, suffix)
      case (Concatenation(as), b) =>
        if (as.head == b) {
          (concatenation(as.tail), Epsilon, List(b), Nil)
        } else if (as.last == b) {
          (concatenation(as.init), Epsilon, Nil, List(b))
        } else {
          (lhs, rhs, Nil, Nil)
        }
      case (a, Concatenation(bs)) =>
        if (bs.head == a) {
          (Epsilon, concatenation(bs.tail), List(a), Nil)
        } else if (bs.last == a) {
          (Epsilon, concatenation(bs.init), Nil, List(a))
        } else {
          (lhs, rhs, Nil, Nil)
        }
      case (a, b) => (a, b, Nil, Nil)
    }
  }

  private def concatenation(ps: List[Pattern]): Pattern = ps match {
    case Nil => Epsilon
    case List(p) => p
    case ps => Concatenation(ps)
  }

  private def findCommonPrefix(lhs: List[Pattern], rhs: List[Pattern]): List[Pattern] = {
    (lhs zip rhs).takeWhile(t => t._1 == t._2).map(_._1)
  }

  private def findCommonSuffix(lhs: List[Pattern], rhs: List[Pattern]): List[Pattern] = {
    findCommonPrefix(lhs.reverse, rhs.reverse).reverse
  }

}

object RegexBuilder {

  sealed trait Pattern
  case object Epsilon extends Pattern
  case class Symbol(value: String) extends Pattern
  case class Alternation(clauses: List[Pattern]) extends Pattern
  case class Concatenation(clauses: List[Pattern]) extends Pattern
  case class Optional(pattern: Pattern) extends Pattern
  case class KleeneStar(pattern: Pattern) extends Pattern
  case class CharSet(chars: List[Symbol]) extends Pattern

}
