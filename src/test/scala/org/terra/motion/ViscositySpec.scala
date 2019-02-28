
package org.terra
package motion

import org.scalatest.{ Matchers, FlatSpec }

import standard._
import standard.motion._
import standard.mass.Kilograms
import standard.time.Seconds
import standard.energy.Joules
import standard.space.{ SquareMeters, Meters, Centimeters, Microns }

/**
 *
 */
class ViscositySpec extends FlatSpec with Matchers with CustomMatchers {

  behavior of "Viscosity and its Units of Measure"

  it should "create values using UOM factories" in {
    PascalSeconds(1).toPascalSeconds should be(1)
    KilogramsPerMeterSecond(1).toKilogramsPerMeterSecond should be(1)
    Poise(1).toPoise should be(1)
    CentiPoise(1).toCentiPoise should be(1)
  }

  it should "create values from properly formatted Strings" in {
    Viscosity("10.22 Pa·s").get should be(PascalSeconds(10.22))
    Viscosity("10.22 kg/m·s").get should be(KilogramsPerMeterSecond(10.22))
    Viscosity("10.22 P").get should be(Poise(10.22))
    Viscosity("10.22 cP").get should be(CentiPoise(10.22))
    Viscosity("10.22 zz").failed.get should be(QuantityParseException("Unable to parse Viscosity", "10.22 zz"))
    Viscosity("zz N").failed.get should be(QuantityParseException("Unable to parse Viscosity", "zz N"))
  }

  it should "properly convert to all supported Units of Measure" in {
    val x = PascalSeconds(1)
    x.toPascalSeconds should be(1)
    x.toKilogramsPerMeterSecond should be(1)
    x.toPoise should be(10)
    x.toCentiPoise should be(1000)
  }

  it should "return properly formatted strings for all supported Units of Measure" in {
    PascalSeconds(1).toString(PascalSeconds) should be("1.0 Pa·s")
    KilogramsPerMeterSecond(1).toString(KilogramsPerMeterSecond) should be("1.0 kg/m·s")
    Poise(1).toString(Poise) should be("1.0 P")
    CentiPoise(1).toString(CentiPoise) should be("1.0 cP")
  }

  it should "return Pressure when divided by Time" in {
    PascalSeconds(1) / Seconds(1) should be(Pascals(1))
  }

  behavior of "ViscosityConversions"

  it should "provide aliases for single unit values" in {
    import ViscosityConversions._

    pascalSecond should be(PascalSeconds(1))
    kilogramPerMeterSecond should be(KilogramsPerMeterSecond(1))
    poise should be(Poise(1))
    centipoise should be(CentiPoise(1))
  }

  it should "provide implicit conversion from Double" in {
    import ViscosityConversions._

    val d = 10d
    d.pascalSeconds should be(PascalSeconds(d))
    d.kilogramsPerMeterSecond should be(KilogramsPerMeterSecond(d))
    d.poise should be(Poise(d))
    d.centipoise should be(CentiPoise(d))
  }

  it should "provide Numeric support" in {
    import ViscosityConversions.ViscosityNumeric

    val fs = List(PascalSeconds(100), PascalSeconds(10))
    fs.sum should be(PascalSeconds(110))
  }
}
