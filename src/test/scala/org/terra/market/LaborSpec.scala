
package org.terra
package market

import org.scalatest.{ Matchers, FlatSpec, TryValues }
import org.terra.CustomMatchers

import org.terra.standard._
import org.terra.standard.market._
import org.terra.standard.time.Hours

/**
  * @author Hunter Payne
  */
class LaborSpec 
    extends FlatSpec with Matchers with CustomMatchers with TryValues {

  behavior of "Labor and its Units of Measure"

  it should "create values using UOM factories" in {

    PersonHours(1).toPersonHours should be(1)
    PersonDays(1).toPersonDays should be(1)
    PersonWeeks(1).toPersonWeeks should be(1)
    PersonYears(1).toPersonYears should be(1)
  }

  it should "create values from properly formatted Strings" in {
    val rate = 10.22
    Labor(s"$rate PHr").get should be(PersonHours(rate))

    Labor(s"$rate PDay").get should be(PersonDays(rate))
    Labor(s"$rate PWk").get should be(PersonWeeks(rate))
    Labor(s"$rate PYr").get should be(PersonYears(rate))

    Labor(s"$rate zz").failed.get should be(
      QuantityParseException("Unable to parse Labor", s"$rate zz"))
    Labor("zz PHr").failed.get should be(
      QuantityParseException("Unable to parse Labor", "zz PHr"))
  }

  it should "properly convert to all supported Units of Measure" in {

    val x = PersonHours(1)

    x.toPersonDays should be(0.125)
    x.toPersonWeeks should be(0.025)
    x.toPersonYears should be(0.0005)
  }

  it should "return properly formatted strings for all supported Units of Measure" in {
    PersonHours(1).toString(PersonHours) should be("1.0 PHr")
    PersonDays(1).toString(PersonDays) should be("1.0 PDay")
    PersonWeeks(1).toString(PersonWeeks) should be("1.0 PWk")
    PersonYears(1).toString(PersonYears) should be("1.0 PYr")
  }

  it should "return Employees when divided by Time" in {
    PersonHours(1) / Hours(1) should be(People(1))
  }

  it should "return a dimensionless ratio when divided by a Labor" in {
    PersonHours(10) / PersonHours(4) should be(2.5)
  }

  behavior of "LaborConversions"

  it should "provide aliases for single unit values" in {
    import LaborConversions._

    workHour should be (PersonHours(1))
    manHour should be (PersonHours(1))
    womanHour should be (PersonHours(1))
    personHour should be (PersonHours(1))

    workDay should be (PersonDays(1))
    manDay should be (PersonDays(1))
    womanDay should be (PersonDays(1))
    personDay should be (PersonDays(1))

    workWeek should be (PersonWeeks(1))
    manWeek should be (PersonWeeks(1))
    womanWeek should be (PersonWeeks(1))
    personWeek should be (PersonWeeks(1))

    workYear should be (PersonYears(1))
    manYear should be (PersonYears(1))
    womanYear should be (PersonYears(1))
    personYear should be (PersonYears(1))
  }

  it should "provide implicit conversion from Double" in {
    import LaborConversions._
    
    val dr = 10l

    dr.personHours should be(PersonHours(10))
    dr.workHours should be(PersonHours(10))
    dr.manHours should be(PersonHours(10))
    dr.womanHours should be(PersonHours(10))

    dr.personDays should be(PersonDays(10))
    dr.workDays should be(PersonDays(10))
    dr.manDays should be(PersonDays(10))
    dr.womanDays should be(PersonDays(10))

    dr.personWeeks should be(PersonWeeks(10))
    dr.workWeeks should be(PersonWeeks(10))
    dr.manWeeks should be(PersonWeeks(10))
    dr.womanWeeks should be(PersonWeeks(10))

    dr.personYears should be(PersonYears(10))
    dr.workYears should be(PersonYears(10))
    dr.manYears should be(PersonYears(10))
    dr.womanYears should be(PersonYears(10))
  }

  it should "provide Numeric support" in {
    import LaborConversions.LaborNumeric

    val dr = List(PersonHours(100), PersonHours(1))
    dr.sum should be(PersonHours(101))
  }
}
