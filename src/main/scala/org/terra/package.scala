
package org

import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal.RoundingMode.RoundingMode

/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

/**
  * ==Terra==
  * A Scala API for Quantities, Units of Measure and Dimensional Analysis
  * with extensible math and data types
  *
  * ==Overview==
  * Terra is a framework of data types and a domain specific language (DSL) 
  * for representing Quantities, 
  * their Units of Measure, and their Dimensional relationships using pluggable
  * types and mathematical operations.
  * The API supports typesafe dimensional analysis, improved domain models 
  * and more.
  * All types are immutable and thread-safe.
  *
  * Typedefs and implicits for common usages
  *
  * @author  garyKeorkunian
  * @author Hunter Payne
  * @version 0.1
  * @since   0.1
  *
  */
package object terra {

  implicit def idConverter[A] = new HasConverter[A, A] {
    override def conv(in: A): A = in
  }

  lazy val isNative: Boolean = false 
  //System.getProperty("java.vm.name") == "Scala.Native"

  // making double to string behave similarily between JVM/JS/Native platforms
  private[terra] def crossFormat[T](d: T)(implicit n: Numeric[T]): String = 
    if (isNative) n.toDouble(d).toString
    else if (n.toLong(d).toDouble == n.toDouble(d))
      "%.1f".format(n.toDouble(d))
    else n.toDouble(d).toString

  import ClassTagType._

  def pseudoClassTagT[T]: PseudoClassTag[T] =
    new PseudoClassTag[T] { val typ = TE }

  def pseudoClassTagTL[T]: PseudoClassTag[T] =
    new PseudoClassTag[T] { val typ = TLE }

  def pseudoClassTagTT[T]: PseudoClassTag[T] =
    new PseudoClassTag[T] { val typ = TTE }

  def pseudoClassTagTC[T]: PseudoClassTag[T] =
    new PseudoClassTag[T] { val typ = TCE }
}
