
package org.terra
package mass

import org.scalatest.{ FlatSpec, Matchers }

import standard._
import standard.mass._
import standard.time.Seconds

class CatalyticActivitySpec extends FlatSpec with Matchers {

  behavior of "CatalyticActivity and its Units of Measure"

  it should "create values using UOM factories" in {
    Katals(1).toKatals should be(1)
    Decikatals(1).toDecikatals should be(1)
    Centikatals(1).toCentikatals should be(1)
    Millikatals(1).toMillikatals should be(1)
    Microkatals(1).toMicrokatals should be(1)
    Nanokatals(1).toNanokatals should be(1)
    Decakatals(1).toDecakatals should be(1)
    Hectokatals(1).toHectokatals should be(1)
    Kilokatals(1).toKilokatals should be(1)
    Megakatals(1).toMegakatals should be(1)
    Gigakatals(1).toGigakatals should be(1)
  }

  it should "create values from properly formatted Strings" in {
    CatalyticActivity("10.22 kat").get should be(Katals(10.22))
    CatalyticActivity("10.22 dkat").get should be(Decikatals(10.22))
    CatalyticActivity("10.22 ckat").get should be(Centikatals(10.22))
    CatalyticActivity("10.22 mkat").get should be(Millikatals(10.22))
    CatalyticActivity("10.22 µkat").get should be(Microkatals(10.22))
    CatalyticActivity("10.22 nkat").get should be(Nanokatals(10.22))
    CatalyticActivity("10.22 dakat").get should be(Decakatals(10.22))
    CatalyticActivity("10.22 hkat").get should be(Hectokatals(10.22))
    CatalyticActivity("10.22 kkat").get should be(Kilokatals(10.22))
    CatalyticActivity("10.22 Mkat").get should be(Megakatals(10.22))
    CatalyticActivity("10.22 Gkat").get should be(Gigakatals(10.22))
    CatalyticActivity("10.45 zz").failed.get should be(QuantityParseException("Unable to parse CatalyticActivity", "10.45 zz"))
    CatalyticActivity("zz kat").failed.get should be(QuantityParseException("Unable to parse CatalyticActivity", "zz kat"))
  }

  it should "properly convert to all supported Units of Measure" in {
    val x = Katals(1)
    val y = Microkatals(0.001)
    x.toKatals should be(1)
    x.toDecikatals should be(10)
    x.toCentikatals should be(100)
    x.toMillikatals should be(1000)
    x.toMicrokatals should be(1000000)
    y.toNanokatals should be(1)
    x.toDecakatals should be(0.1)
    x.toHectokatals should be(0.01)
    x.toKilokatals should be(0.001)
    x.toMegakatals should be(0.000001)
    x.toGigakatals should be(0.000000001)
  }

  it should "return properly formatted strings for all supported Units of Measure" in {
    Katals(1).toString(Katals) should be("1.0 kat")
    Decikatals(1).toString(Decikatals) should be("1.0 dkat")
    Centikatals(1).toString(Centikatals) should be("1.0 ckat")
    Millikatals(1).toString(Millikatals) should be("1.0 mkat")
    Microkatals(1).toString(Microkatals) should be("1.0 µkat")
    Nanokatals(1).toString(Nanokatals) should be("1.0 nkat")
    Decakatals(1).toString(Decakatals) should be("1.0 dakat")
    Hectokatals(1).toString(Hectokatals) should be("1.0 hkat")
    Kilokatals(1).toString(Kilokatals) should be("1.0 kkat")
    Megakatals(1).toString(Megakatals) should be("1.0 Mkat")
    Gigakatals(1).toString(Gigakatals) should be("1.0 Gkat")
  }

  it should "return ChemicalAmount when multiplied by Time" in {
    Katals(1) * Seconds(1) should be(Moles(1))
  }

  behavior of "CatalyticActivityConversions"

  it should "provide aliases for single unit values" in {
    import CatalyticActivityConversions._

    katal should be(Katals(1))
    decikatal should be(Decikatals(1))
    centikatal should be(Centikatals(1))
    millikatal should be(Millikatals(1))
    microkatal should be(Microkatals(1))
    nanokatal should be(Nanokatals(1))
    decakatal should be(Decakatals(1))
    hectokatal should be(Hectokatals(1))
    kilokatal should be(Kilokatals(1))
    megakatal should be(Megakatals(1))
    gigakatal should be(Gigakatals(1))
  }

  it should "provide implicit conversion from Double" in {
    import CatalyticActivityConversions._

    val d = 10d
    d.katals should be(Katals(d))
    d.decikatals should be(Decikatals(d))
    d.centikatals should be(Centikatals(d))
    d.millikatals should be(Millikatals(d))
    d.microkatals should be(Microkatals(d))
    d.nanokatals should be(Nanokatals(d))
    d.decakatals should be(Decakatals(d))
    d.hectokatals should be(Hectokatals(d))
    d.kilokatals should be(Kilokatals(d))
    d.megakatals should be(Megakatals(d))
    d.gigakatals should be(Gigakatals(d))
  }

  it should "provide Numeric support" in {
    import CatalyticActivityConversions.CatalyticActivityNumeric

    val cas = List(Katals(100), Katals(10))
    cas.sum should be(Katals(110))
  }
}
