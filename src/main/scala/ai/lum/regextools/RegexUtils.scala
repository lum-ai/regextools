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

import scala.language.implicitConversions

object RegexUtils {

  val metacharacters = "<([{\\^-=$!|]})?*+.>"

  /** Returns a literal pattern String for the specified String.
   *  This method differs from scala.util.matching.Regex.quote()
   *  in that it adds backslashes to regex metacharacters instead
   *  of surrounding the string with \Q and \E
   */
  def quote(s: String): String = {
    s.map(c => if (metacharacters contains c) s"\\$c" else c).mkString
  }

  /** returns true if the c is a regex metacharacter */
  def isRegexMetaCharacter(c: Char): Boolean = {
    metacharacters.contains(c)
  }

  /** Makes regexes for bracketed strings.
   *  pairs: "(){}[]<>"
   */
  def mkCharBracketed(pairs: String, escapes: String = "\\"): String = {
    implicit def asPair(a: Array[String]): (String, String) = (a(0), a(1))
    if (pairs.isEmpty) throw new IllegalArgumentException("missing pairs")
    if (pairs.size % 2 != 0) throw new IllegalArgumentException("incomplete pair")
    val (open, close) = pairs.split("").grouped(2).toArray.unzip
    mkCharDelimited(open.mkString, escapes, close.mkString)
  }

  /** Makes regexes for delimited strings.
   *  Uses non-capturing groups and possessive quantifiers for efficiency.
   */
  def mkCharDelimited(
      delimiters: String,
      escapes: String = "\\",
      closeDelimiters: String = ""
  ): String = {
    require(delimiters.nonEmpty, "missing delimirers")
    val numDelimiters = delimiters.size
    // get escape chars
    val escs = escapes.size match {
      case 0 => "" // no escape char
      case 1 => escapes * numDelimiters // same escape char for all delimiters
      case `numDelimiters` => escapes // distinct escape char per delimiter
      case _ => throw new IllegalArgumentException("invalid escapes")
    }
    // get close delimiters
    val cdels = closeDelimiters.size match {
      case 0 => delimiters // close delimiters default to open delimiters
      case `numDelimiters` => closeDelimiters // distinct close delimiter per open delimiter
      case _ => throw new IllegalArgumentException("illegal close delimiters")
    }
    // get patterns for each open/close delimiter pair
    val patterns = for (i <- delimiters.indices) yield {
      // quote chars for regex compatibility
      val del = quote(delimiters(i).toString)
      val cdel = quote(cdels(i).toString)
      val esc = if (escs.nonEmpty) quote(escs(i).toString) else ""
      // make pattern
      if (cdel == esc) {
        // escape is equal to close delimiter
        s"$del[^$cdel]*+(?:$cdel$cdel[^$cdel]*+)*+$cdel"
      } else if (esc.nonEmpty) {
        // escape char can be used on to escape any char
        s"$del[^$esc$cdel]*+(?:$esc.[^$esc$cdel]*+)*+$cdel"
      } else {
        // no escape char
        s"$del[^$cdel]*+$cdel"
      }
    }
    // return single regex for all patterns
    patterns.mkString("|")
  }

}
