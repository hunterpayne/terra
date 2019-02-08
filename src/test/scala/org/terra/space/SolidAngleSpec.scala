/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package space

import org.scalatest.{ FlatSpec, Matchers }
import standard._
import standard.space._
import standard.photo.{ Candelas, Lumens }

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 */
class SolidAngleSpec extends FlatSpec with Matchers {

  behavior of "SolidAngle and its Units of Measure"

  it should "create values using UOM factories" in {
    SquareRadians(1).toSquareRadians should be(1)
  }

  it should "create values from properly formatted Strings" in {
    SolidAngle("10.22 sr").get should be(SquareRadians(10.22))
    SolidAngle("10.22 zz").failed.get should be(QuantityParseException("Unable to parse SolidAngle", "10.22 zz"))
    SolidAngle("ZZ sr").failed.get should be(QuantityParseException("Unable to parse SolidAngle", "ZZ sr"))
  }

  it should "properly convert to all supported Units of Measure" in {
    val x = SquareRadians(1)
    x.toSquareRadians should be(1)
  }

  it should "return properly formatted strings for all supported Units of Measure" in {
    SquareRadians(1).toString should be("1.0 sr")
  }

  it should "return LuminousFlux when multiplied by LuminousIntensity" in {
    SquareRadians(1) * Candelas(1) == Lumens(1)
  }

  behavior of "SolidAngleConversion"

  it should "provide aliases for single unit values" in {
    import SolidAngleConversions._

    squareRadian should be(SquareRadians(1))
    steradian should be(SquareRadians(1))
  }
  it should "provide implicit conversion from Double" in {
    import SolidAngleConversions._

    val d = 10d
    d.squareRadians should be(SquareRadians(d))
    d.steradians should be(SquareRadians(d))
  }

  it should "provide Numeric support" in {
    import SolidAngleConversions.SolidAngleNumeric

    val sas = List(SquareRadians(100), SquareRadians(1))
    sas.sum should be(SquareRadians(101))
  }
}
