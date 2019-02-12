
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
class EmployeeSpec 
    extends FlatSpec with Matchers with CustomMatchers with TryValues {

  behavior of "Employees and their Units of Measure"

  it should "create values using UOM factories" in {

    People(1).toPeople should be(1)
  }

  it should "create values from properly formatted Strings" in {
    val number = 11l
    Employee(s"$number P").get should be(People(number))

    Employee(s"$number zz").failed.get should be(
      QuantityParseException("Unable to parse Employee", s"$number zz"))
    Employee("zz P").failed.get should be(
      QuantityParseException("Unable to parse Employee", "zz P"))
  }

  it should "return properly formatted strings for all supported Units of Measure" in {

    People(1).toString(People) should be("1.0 P")
  }

  it should "return Labor when multiplied by Time" in {
    People(1) * Hours(1) should be(PersonHours(1))
  }

  it should "return a dimensionless ratio when divided by an Employee" in {
    People(10) / People(4) should be(2.5)
  }

  behavior of "EmployeeConversions"

  it should "provide aliases for single unit values" in {
    import EmployeeConversions._

    person should be(People(1))
  }

  it should "provide implicit conversion from Double" in {
    import EmployeeConversions._
    
    val dr = 10l
    dr.people should be(People(10))
  }

  it should "provide Numeric support" in {
    import EmployeeConversions.EmployeeNumeric

    val dr = List(People(100), People(1))
    dr.sum should be(People(101))
  }
}
