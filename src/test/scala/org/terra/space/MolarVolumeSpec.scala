
package org.terra
package space

import org.scalatest.{ FlatSpec, Matchers }
import standard._
import standard.space._
import standard.energy.{ Joules, JoulesPerCubicMeter }
import standard.mass.{ Kilograms, KilogramsPerCubicMeter, Moles }
import standard.motion.CubicMetersPerSecond
import standard.time.Seconds

/**
 *
 */
class MolarVolumeSpec extends FlatSpec with Matchers {

  behavior of "MolarVolume and its Units of Measure"

  it should "create values using UOM factories" in {

    CubicMetersPerMole(1).toCubicMetersPerMole should be(1)
  }

  it should "create values from properly formatted Strings" in {
    MolarVolume("10.22 m³/mol").get should be(CubicMetersPerMole(10.22))
    MolarVolume("10.22 zz").failed.get should be(QuantityParseException("Unable to parse MolarVolume", "10.22 zz"))
    MolarVolume("ZZ m³/mol").failed.get should be(QuantityParseException("Unable to parse MolarVolume", "ZZ m³/mol"))
  }

  it should "properly convert to all supported Units of Measure" in {
    val x = CubicMetersPerMole(1)

    x.toCubicMetersPerMole should be(1)
  }

  it should "return properly formatted strings for all supported Units of Measure" in {
    CubicMetersPerMole(1).toString(CubicMetersPerMole) should be("1.0 m³/mol")
  }

  it should "return Volume when multiplied by ChemicalAmount" in {
    CubicMetersPerMole(1) * Moles(1) should be(CubicMeters(1))
  }

  behavior of "MolarVolumeConversions"

  it should "provide aliases for single unit values" in {
    import MolarVolumeConversions._

    cubicMeterPerMole should be(CubicMetersPerMole(1))
  }

  it should "provide implicit conversion from Double" in {
    import MolarVolumeConversions._

    val d = 10d

    d.cubicMetersPerMole should be(CubicMetersPerMole(d))
  }

  it should "provide Numeric support" in {
    import MolarVolumeConversions.MolarVolumeNumeric

    val vs = List(CubicMetersPerMole(100), CubicMetersPerMole(1))
    vs.sum should be(CubicMetersPerMole(101))
  }
}
