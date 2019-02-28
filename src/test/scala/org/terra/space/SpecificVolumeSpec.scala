
package org.terra
package space

import org.scalatest.{ FlatSpec, Matchers }
import standard._
import standard.space._
import standard.energy.{ Joules, JoulesPerCubicMeter }
import standard.mass.{ Kilograms, KilogramsPerCubicMeter }
import standard.motion.CubicMetersPerSecond
import standard.time.Seconds

/**
 *
 */
class SpecificVolumeSpec extends FlatSpec with Matchers {

  behavior of "SpecificVolume and its Units of Measure"

  it should "create values using UOM factories" in {

    CubicMetersPerKilogram(1).toCubicMetersPerKilogram should be(1)
    MillilitresPerGram(1).toMillilitresPerGram should be(1)
    CubicFeetPerPound(1).toCubicFeetPerPound should be(1)
    CubicFeetPerSlug(1).toCubicFeetPerSlug should be(1)
  }

  it should "create values from properly formatted Strings" in {
    SpecificVolume("10.22 m³/kg").get should be(CubicMetersPerKilogram(10.22))
    SpecificVolume("10.22 ml/g").get should be(MillilitresPerGram(10.22))
    SpecificVolume("10.22 ft³/lb").get should be(CubicFeetPerPound(10.22))
    SpecificVolume("10.22 ft³/slug").get should be(CubicFeetPerSlug(10.22))
    SpecificVolume("10.22 zz").failed.get should be(QuantityParseException("Unable to parse SpecificVolume", "10.22 zz"))
    SpecificVolume("ZZ m³/kg").failed.get should be(QuantityParseException("Unable to parse SpecificVolume", "ZZ m³/kg"))
  }

  it should "properly convert to all supported Units of Measure" in {
    val x = CubicMetersPerKilogram(1)

    x.toCubicMetersPerKilogram should be(1)
    x.toMillilitresPerGram should be(1)
    x.toCubicFeetPerPound should be(16.018367263564336)
    x.toCubicFeetPerSlug should be(515.3756224068134)
  }

  it should "return properly formatted strings for all supported Units of Measure" in {
    CubicMetersPerKilogram(1).toString(CubicMetersPerKilogram) should be("1.0 m³/kg")
    MillilitresPerGram(1).toString(MillilitresPerGram) should be("1.0 ml/g")
    CubicFeetPerPound(1).toString(CubicFeetPerPound) should be("1.0 ft³/lb")
    CubicFeetPerSlug(1).toString(CubicFeetPerSlug) should be("1.0 ft³/slug")
  }

  it should "return Volume when multiplied by Mass" in {
    CubicMetersPerKilogram(1) * Kilograms(1) should be(CubicMeters(1))
  }

  behavior of "SpecificVolumeConversions"

  it should "provide aliases for single unit values" in {
    import SpecificVolumeConversions._

    cubicMeterPerKilogram should be(CubicMetersPerKilogram(1))
    millilitrePerGram should be(MillilitresPerGram(1))
    cubicFootPerPound should be(CubicFeetPerPound(1))
    cubicFootPerSlug should be(CubicFeetPerSlug(1))
  }

  it should "provide implicit conversion from Double" in {
    import SpecificVolumeConversions._

    val d = 10d

    d.cubicMetersPerKilogram should be(CubicMetersPerKilogram(d))
    d.millilitresPerGram should be(MillilitresPerGram(d))
    d.cubicFeetPerPound should be(CubicFeetPerPound(d))
    d.cubicFeetPerSlug should be(CubicFeetPerSlug(d))
  }

  it should "provide Numeric support" in {
    import SpecificVolumeConversions.SpecificVolumeNumeric

    val vs = List(CubicMetersPerKilogram(100), CubicMetersPerKilogram(1))
    vs.sum should be(CubicMetersPerKilogram(101))
  }
}
