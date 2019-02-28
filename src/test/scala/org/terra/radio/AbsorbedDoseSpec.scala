
package org.terra
package radio

import org.scalatest.{ Matchers, FlatSpec }

import standard._
import standard.radio._
import standard.time._
import standard.mass.Kilograms
import standard.energy.Joules

/**
 *
 */
class AbsorbedDoseSpec extends FlatSpec with Matchers {

  behavior of "AbsorbedDose and its Units of Measure"

  it should "create values using UOM factories" in {

    Grays(1).toGrays should be(1)
    Rads(1).toRads should be(1)
    ErgsPerGram(1).toErgsPerGram should be(1)
  }

  it should "create values from properly formatted Strings" in {
    AbsorbedDose("10.22 Gy").get should be(Grays(10.22))
    AbsorbedDose("8.47 rad").get should be(Rads(8.47))
    AbsorbedDose("6.79 erg/g").get should be(ErgsPerGram(6.79))
    AbsorbedDose("10.22 zz").failed.get should be(QuantityParseException("Unable to parse AbsorbedDose", "10.22 zz"))
    AbsorbedDose("ZZ Gy").failed.get should be(QuantityParseException("Unable to parse AbsorbedDose", "ZZ Gy"))
  }

  it should "properly convert to all supported Units of Measure" in {

    val x = Grays(1)
    x.toGrays should be(1)
    x.toRads should be(100)
    x.toErgsPerGram should be(10000)

    val y = Rads(1)
    y.toRads should be(1)
    y.toGrays should be(0.01)
    y.toErgsPerGram should be(100)

    val z = ErgsPerGram(1)
    z.toErgsPerGram should be(1)
    z.toGrays should be(0.0001)
    z.toRads should be(0.01)
  }

  it should "return properly formatted strings for all supported Units of Measure" in {
    Grays(1).toString(Grays) should be("1.0 Gy")
    Rads(1).toString(Rads) should be("1.0 rad")
    ErgsPerGram(1).toString(ErgsPerGram) should be("1.0 erg/g")
  }

  it should "return Energy when multiplied by Mass" in {
    Grays(1) * Kilograms(10) should be(Joules(10))
    Rads(1) * Kilograms(10) should be(Joules(0.1))
    ErgsPerGram(1) * Kilograms(10) should be(Joules(0.001))
  }

  behavior of "Conversions"

  it should "provide aliases for single unit values" in {
    import AbsorbedDoseConversions._

    gray should be(Grays(1))
    rad should be(Rads(1))
    ergsPerGram should be(ErgsPerGram(1))
  }

  it should "provide implicit conversion from Double" in {
    import AbsorbedDoseConversions._

    val d = 10d
    d.grays should be(Grays(d))
    d.rads should be(Rads(d))
    d.ergsPerGram should be(ErgsPerGram(d))
  }

  it should "provide Numeric support" in {
    import AbsorbedDoseConversions.AbsorbedDoseNumeric

    val ses = List(Grays(100), Grays(10))
    ses.sum should be(Grays(110))

    val sesRad = List(Rads(100), Rads(10))
    sesRad.sum should be(Rads(110))

    val sesErg = List(ErgsPerGram(100), ErgsPerGram(10))
    sesErg.sum should be(Grays(0.011))
  }
}
