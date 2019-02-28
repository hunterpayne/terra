
package org.terra
package mass

import org.scalatest.{ FlatSpec, Matchers }

import standard._
import standard.mass._
import standard.space.{ CubicMeters, CubicMetersPerMole }

class ConcentrationSpec extends FlatSpec with Matchers {

  behavior of "Concentration and its Units of Measure"

  it should "create values using UOM factories" in {
    MolesPerCubicMeter(1).toMolesPerCubicMeter should be(1)
  }

  it should "create values from properly formatted Strings" in {
    Concentration("10.22 mol/m³").get should be(MolesPerCubicMeter(10.22))
    Concentration("10.45 zz").failed.get should be(QuantityParseException("Unable to parse Concentration", "10.45 zz"))
    Concentration("zz mol/m³").failed.get should be(QuantityParseException("Unable to parse Concentration", "zz mol/m³"))
  }

  it should "properly convert to all supported Units of Measure" in {
    val x = MolesPerCubicMeter(1)
    x.toMolesPerCubicMeter should be(1)
  }

  it should "return properly formatted strings for all supported Units of Measure" in {
    MolesPerCubicMeter(1).toString(MolesPerCubicMeter) should be("1.0 mol/m³")
  }

  it should "when multiplied by a Volume return a ChemicalAmount" in {
    MolesPerCubicMeter(1) * CubicMeters(1) should be(Moles(1))
  }

  it should "return MolarVolume when inverted" in {
    MolesPerCubicMeter(0.1).inv should be(CubicMetersPerMole(10))
  }

  behavior of "ConcentrationConversions"

  it should "provide aliases for single unit values" in {
    import ConcentrationConversions._

    molePerCubicMeter should be(MolesPerCubicMeter(1))
  }

  it should "provide implicit conversion from Double" in {
    import ConcentrationConversions._

    val d = 10d
    d.molesPerCubicMeter should be(MolesPerCubicMeter(d))
  }

  it should "provide Numeric support" in {
    import ConcentrationConversions.ConcentrationNumeric

    val cas = List(MolesPerCubicMeter(100), MolesPerCubicMeter(10))
    cas.sum should be(MolesPerCubicMeter(110))
  }
}
