
package org

import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal.RoundingMode.RoundingMode
import scala.reflect.{ ClassTag, classTag }

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

  // TODO JS/Native support
  private[terra] def crossFormat[T](d: T)(implicit n: Numeric[T]): String = 
    if (n.toLong(d).toDouble == n.toDouble(d)) n.toLong(d).toString + ".0"
    else n.toDouble(d).toString
}
