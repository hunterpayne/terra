
package org.terra
package energy

import org.scalatest.{ Matchers, FlatSpec }

import standard._
import standard.energy._
import standard.space.SquareMeters

/**
 *
 */
class EnergyAreaDensitySpec extends FlatSpec with Matchers {

  behavior of "EnergyAreaDensity and its Units of Measure"

  it should "create values using UOM factories" in {
    JoulesPerSquareMeter(1).toJoulesPerSquareMeter should be(1)
  }

  it should "create values from properly formatted Strings" in {
    EnergyAreaDensity("10.22 j/m²").get should be(JoulesPerSquareMeter(10.22))
    EnergyAreaDensity("10.22 zz").failed.get should be(QuantityParseException("Unable to parse EnergyAreaDensity", "10.22 zz"))
    EnergyAreaDensity("ZZ j/m²").failed.get should be(QuantityParseException("Unable to parse EnergyAreaDensity", "ZZ j/m²"))
  }

  it should "properly convert to all supported Units of Measure" in {
    val x = JoulesPerSquareMeter(1)

    x.toJoulesPerSquareMeter should be(1)
  }

  it should "return properly formatted strings for all supported Units of Measure" in {
    JoulesPerSquareMeter(1).toString(JoulesPerSquareMeter) should be("1.0 j/m²")
  }

  it should "return Energy when multiplied by Volume" in {
    JoulesPerSquareMeter(1) * SquareMeters(10) should be(Joules(10))
  }

  behavior of "EnergyAreaDensityConversions"

  it should "provide aliases for single unit values" in {
    import EnergyAreaDensityConversions._

    joulePerSquareMeter should be(JoulesPerSquareMeter(1))
  }

  it should "provide implicit conversion from Double" in {
    import EnergyAreaDensityConversions._

    val d = 10.22d
    d.joulesPerSquareMeter should be(JoulesPerSquareMeter(d))
  }

  it should "provide Numeric support" in {
    import EnergyAreaDensityConversions.EnergyAreaDensityNumeric

    val eds = List(JoulesPerSquareMeter(10), JoulesPerSquareMeter(100))
    eds.sum should be(JoulesPerSquareMeter(110))
  }
}
