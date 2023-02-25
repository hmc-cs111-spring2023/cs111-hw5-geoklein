package regex

/** *****************************************************************************
  * Regular Languages
  *
  * data structure definitions for regular languages
  */

trait RegularLanguage

case object Empty extends RegularLanguage

case object Epsilon extends RegularLanguage

case class Character(c: Char) extends RegularLanguage

case class Union(exp1: RegularLanguage, exp2: RegularLanguage) extends RegularLanguage

case class Concat(exp1: RegularLanguage, exp2: RegularLanguage) extends RegularLanguage

case class Star(exp: RegularLanguage) extends RegularLanguage

/** *****************************************************************************
  * Derivatives
  *
  * Fill in the function definitions below
  */

/** Simplifies a regular language */
def simplify(lang: RegularLanguage): RegularLanguage = lang match
  case Concat(Epsilon, exp2) => simplify(exp2)
  case Concat(exp1, Epsilon) => simplify(exp1)
  case Concat(Empty, exp2) => Empty
  case Concat(exp1, Empty) => Empty
  case Concat(exp1, exp2) => Concat(simplify(exp1), simplify(exp2))

  case Union(exp1, Empty) => simplify(exp1)
  case Union(Empty, exp2) => simplify(exp2)
  case Union(exp1, exp2) => Union(simplify(exp1), simplify(exp2))

  case Star(Epsilon) => Epsilon
  case Star(Empty) => Empty
  case Star(exp) => Star(simplify(exp))
  
  // Fall through case.
  case _ => lang


/** A language is nullable if it contains ε */
def nullable(lang: RegularLanguage): Boolean = simplify(lang) match
  case Epsilon => true
  case Star(_) => true

  case Union(exp1, exp2) => nullable(exp1) || nullable(exp2)
  case Concat(exp1, exp2) => nullable(exp1) && nullable(exp2)

  case _ => false

/** Computes the derivative of a language, with respect to a character */
def derivative(l: RegularLanguage)(c: Char): RegularLanguage = l match
  case Empty => Empty
  case Epsilon => Empty

  case Character(d) => if (c == d) then Epsilon else Empty

  case Union(exp1, exp2) => Union(derivative(exp1)(c), derivative(exp2)(c))
  case Concat(exp1, exp2) => if !nullable(exp1) then Concat(derivative(exp1)(c), exp2)
                             else Union(Concat(derivative(exp1)(c), exp2), derivative(exp2)(c))
  case Star(exp) => Concat(derivative(exp)(c), Star(exp))

/** *****************************************************************************
  * String-matching with regular expressions
  */

/** Given a string s and a language l, returns true if the string matches the
  * pattern described by l
  */
def matches(s: String, l: RegularLanguage): Boolean =
  if (s.isEmpty) then nullable(l)
  else matches(s.tail, derivative(l)(s.head))
