
package org.terra
package energy

import org.scalatest.{ Matchers, FlatSpec }

import standard._
import standard.energy._
import standard.time._
import standard.mass.Kilograms

/**
 *
 */
class SpecificEnergySpec extends FlatSpec with Matchers {

  behavior of "SpecificEnergy and its Units of Measure"

  it should "create values using UOM factories" in {

    JoulesPerKilogram(1).toJoulesPerKilogram should be(1)
  }

  it should "create values from properly formatted Strings" in {
    SpecificEnergy("10.22 J/kg").get should be(JoulesPerKilogram(10.22))
    SpecificEnergy("10.22 zz").failed.get should be(QuantityParseException("Unable to parse SpecificEnergy", "10.22 zz"))
    SpecificEnergy("ZZ J/kg").failed.get should be(QuantityParseException("Unable to parse SpecificEnergy", "ZZ J/kg"))
  }

  it should "properly convert to all supported Units of Measure" in {

    val x = JoulesPerKilogram(1)
    x.toJoulesPerKilogram should be(1)
  }

  it should "return properly formatted strings for all supported Units of Measure" in {
    JoulesPerKilogram(1).toString(JoulesPerKilogram) should be("1.0 J/kg")
  }

  it should "return Energy when multiplied by Mass" in {
    JoulesPerKilogram(1) * Kilograms(10) should be(Joules(10))
  }

  behavior of "Conversions"

  it should "provide aliases for single unit values" in {
    import SpecificEnergyConversions._

    joulePerKilogram should be(JoulesPerKilogram(1))
  }

  it should "provide implicit conversion from Double" in {
    import SpecificEnergyConversions._

    val d = 10d
    d.joulesPerKilogram should be(JoulesPerKilogram(d))
  }

  it should "provide Numeric support" in {
    import SpecificEnergyConversions.SpecificEnergyNumeric

    val ses = List(JoulesPerKilogram(100), JoulesPerKilogram(10))
    ses.sum should be(JoulesPerKilogram(110))
  }
}
