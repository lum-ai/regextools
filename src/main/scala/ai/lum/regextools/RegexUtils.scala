package ai.lum.regextools

import scala.util.matching.Regex
import scala.language.implicitConversions

object RegexUtils {

  val specialChars = "<([{\\^-=$!|]})?*+.>"

  def quote(s: String): String = {
    s.map(c => if (specialChars contains c) s"\\$c" else c).mkString
  }

  // pairs: "(){}[]<>"
  def mkCharBracketed(pairs: String, escapes: String = "\\"): Regex = {
    implicit def asPair(a: Array[String]): (String, String) = (a(0), a(1))
    require(pairs.nonEmpty, "missing pairs")
    require(pairs.size % 2 == 0, "incomplete pair")
    val (open, close) = pairs.split("").grouped(2).toArray.unzip
    mkCharDelimited(open.mkString, close.mkString, escapes)
  }

  def mkCharDelimited(
      delimiters: String,
      closeDelimiters: String = "",
      escapes: String = "\\"
  ): Regex = {
    require(delimiters.nonEmpty, "missing delimirers")
    val numDelimiters = delimiters.size
    // get escape chars
    val escs = escapes.size match {
      case 0 => "" // no escape char
      case 1 => escapes * numDelimiters // same escape char for all delimiters
      case `numDelimiters` => escapes // distinct escape char per delimiter
      case _ => throw new Exception
    }
    // get close delimiters
    val cdels = closeDelimiters.size match {
      case 0 => delimiters // close delimiters default to open delimiters
      case `numDelimiters` => closeDelimiters // distinct close delimiter per open delimiter
      case _ => throw new Exception
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
        s"$del[^$cdel]*(?:$cdel$cdel[^$cdel]*)*$cdel"
      } else if (esc.nonEmpty) {
        // escape char can be used on to escape any char
        s"$del[^$esc$cdel]*(?:$esc.[^$esc$cdel]*)*$cdel"
      } else {
        // no escape char
        s"$del[^$cdel]*$cdel"
      }
    }
    // return single regex for all patterns
    patterns.mkString("|").r
  }

}
