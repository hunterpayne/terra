
package org.terra
package mass

import org.scalatest.{ FlatSpec, Matchers }

import standard._
import standard.mass._

/**
 *
 */
class MolaritySpec extends FlatSpec with Matchers {

  behavior of "Molarity and its Units of Measure"

  it should "create values using UOM factories" in {
    MolesPerKilogram(1).toMolesPerKilogram should be(1)
  }

  it should "create values from properly formatted Strings" in {
    Molarity("10.22 mol/kg").get should be(MolesPerKilogram(10.22))
    Molarity("10.45 zz").failed.get should be(QuantityParseException("Unable to parse Molarity", "10.45 zz"))
    Molarity("zz mol/kg").failed.get should be(QuantityParseException("Unable to parse Molarity", "zz mol/kg"))
  }

  it should "properly convert to all supported Units of Measure" in {
    val x = MolesPerKilogram(1)
    x.toMolesPerKilogram should be(1)
  }

  it should "return properly formatted strings for all supported Units of Measure" in {
    MolesPerKilogram(1).toString(MolesPerKilogram) should be("1.0 mol/kg")
  }

  it should "return ChemicalAmount when multiplied by Mass" in {
    MolesPerKilogram(1) * Kilograms(1) should be(Moles(1))
  }

  it should "return MolarMass when inverted" in {
    MolesPerKilogram(0.1).inv should be(KilogramsPerMole(10))
  }

  behavior of "MolarityConversions"

  it should "provide aliases for single unit values" in {
    import MolarityConversions._

    molePerKilogram should be(MolesPerKilogram(1))
  }

  it should "provide implicit conversion from Double" in {
    import MolarityConversions._

    val d = 10d
    d.molesPerKilogram should be(MolesPerKilogram(d))
  }

  it should "provide Numeric support" in {
    import MolarityConversions.MolarityNumeric

    val cas = List(MolesPerKilogram(100), MolesPerKilogram(10))
    cas.sum should be(MolesPerKilogram(110))
  }
}
