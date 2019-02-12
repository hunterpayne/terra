
package org.terra
package mass

import org.scalatest.{ FlatSpec, Matchers }

import standard._
import standard.mass._

/**
 *
 */
class MolarMassSpec extends FlatSpec with Matchers {

  behavior of "MolarMass and its Units of Measure"

  it should "create values using UOM factories" in {
    KilogramsPerMole(1).toKilogramsPerMole should be(1)
    GramsPerMole(1).toGramsPerMole should be(1)
  }

  it should "create values from properly formatted Strings" in {
    MolarMass("10.22 kg/mol").get should be(KilogramsPerMole(10.22))
    MolarMass("10.22 g/mol").get should be(GramsPerMole(10.22))
    MolarMass("10.45 zz").failed.get should be(QuantityParseException("Unable to parse MolarMass", "10.45 zz"))
    MolarMass("zz kg/mol").failed.get should be(QuantityParseException("Unable to parse MolarMass", "zz kg/mol"))
  }

  it should "properly convert to all supported Units of Measure" in {
    val x = KilogramsPerMole(1)
    x.toKilogramsPerMole should be(1)
    x.toGramsPerMole should be(1000)
  }

  it should "return properly formatted strings for all supported Units of Measure" in {
    KilogramsPerMole(1).toString(KilogramsPerMole) should be("1.0 kg/mol")
    GramsPerMole(1).toString(GramsPerMole) should be("1.0 g/mol")
  }

  it should "return Mass when multiplied by ChemicalAmount" in {
    KilogramsPerMole(1) * Moles(1) should be(Kilograms(1))
  }

  behavior of "MolarMassConversions"

  it should "provide aliases for single unit values" in {
    import MolarMassConversions._

    kilogramPerMole should be(KilogramsPerMole(1))
    gramPerMole should be(GramsPerMole(1))
  }

  it should "provide implicit conversion from Double" in {
    import MolarMassConversions._

    val d = 10d
    d.kilogramsPerMole should be(KilogramsPerMole(d))
    d.gramsPerMole should be(GramsPerMole(d))
  }

  it should "provide Numeric support" in {
    import MolarMassConversions.MolarMassNumeric

    val cas = List(KilogramsPerMole(100), KilogramsPerMole(10))
    cas.sum should be(KilogramsPerMole(110))
  }
}
