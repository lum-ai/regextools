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

object RegexAst {

  sealed trait Pattern
  case object Epsilon extends Pattern
  case class Symbol(value: String) extends Pattern
  case class Alternation(clauses: List[Pattern]) extends Pattern
  case class Concatenation(clauses: List[Pattern]) extends Pattern
  case class Optional(pattern: Pattern) extends Pattern
  case class KleeneStar(pattern: Pattern) extends Pattern
  case class CharSet(chars: List[Symbol]) extends Pattern

  case class Quantifier(min: Int, max: Option[Int]) {
    require(
      max.isEmpty || min <= max.get,
      s"min=${min} should't be greater than max=${max.get}"
    )
  }

}
