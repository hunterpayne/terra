
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
class SurfaceTensionSpec extends FlatSpec with Matchers with CustomMatchers {

  behavior of "SurfaceTension and its Units of Measure"

  it should "create values using UOM factories" in {
    NewtonsPerMeter(1).toNewtonsPerMeter should be(1)
    PoundsPerFoot(1).toPoundsPerFoot should be(1)
  }

  it should "create values from properly formatted Strings" in {
    SurfaceTension("10.22 N/m").get should be(NewtonsPerMeter(10.22))
    SurfaceTension("10.22 lb/ft").get should be(PoundsPerFoot(10.22))
    SurfaceTension("10.22 zz").failed.get should be(QuantityParseException("Unable to parse SurfaceTension", "10.22 zz"))
    SurfaceTension("zz N").failed.get should be(QuantityParseException("Unable to parse SurfaceTension", "zz N"))
  }

  it should "properly convert to all supported Units of Measure" in {
    val x = NewtonsPerMeter(1)
    x.toNewtonsPerMeter should be(1)
    x.toPoundsPerFoot should be(0.06852190290032348)
  }

  it should "return properly formatted strings for all supported Units of Measure" in {
    NewtonsPerMeter(1).toString(NewtonsPerMeter) should be("1.0 N/m")
    PoundsPerFoot(1).toString(PoundsPerFoot) should be("1.0 lb/ft")
  }

  it should "return Force when multiplied by Length" in {
    NewtonsPerMeter(1) * Meters(1) should be(Newtons(1))
  }

  behavior of "SurfaceTensionConversions"

  it should "provide aliases for single unit values" in {
    import SurfaceTensionConversions._

    newtonPerMeter should be(NewtonsPerMeter(1))
    poundPerFoot should be(PoundsPerFoot(1))
  }

  it should "provide implicit conversion from Double" in {
    import SurfaceTensionConversions._

    val d = 10d
    d.newtonsPerMeter should be(NewtonsPerMeter(d))
    d.poundsPerFoot should be(PoundsPerFoot(d))
  }

  it should "provide Numeric support" in {
    import SurfaceTensionConversions.SurfaceTensionNumeric

    val fs = List(NewtonsPerMeter(100), NewtonsPerMeter(10))
    fs.sum should be(NewtonsPerMeter(110))
  }
}
