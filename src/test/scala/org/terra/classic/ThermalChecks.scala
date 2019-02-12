
package org.terra.classic

import org.scalacheck.Properties
import org.scalacheck.Prop._

import thermal._
import energy.Joules

/**
 *
 */
object ThermalChecks extends Properties("Thermal") with QuantityChecks {

  property("Celsius to Fahrenheit") = forAll { (a: Double) ⇒
    Celsius(a).toFahrenheitScale == (a * 9d / 5) + 32d
  }

  property("Celsius to Kelvin") = forAll { (a: Double) ⇒
    Celsius(a).toKelvinScale == a + 273.15
  }

  property("Fahrenheit to Celsius ") = forAll { (a: Double) ⇒
    Fahrenheit(a).toCelsiusScale == (a - 32d) * 5d / 9d
  }

  property("Fahrenheit to Kelvin ") = forAll { (a: Double) ⇒
    Fahrenheit(a).toKelvinScale == (a + 459.67) * 5d / 9d
  }

  property("Kelvin to Fahrenheit") = forAll { (a: Double) ⇒
    Kelvin(a).toFahrenheitScale == (a * 9d / 5) - 459.67
  }

  property("Kelvin to Celsius") = forAll { (a: Double) ⇒
    Kelvin(a).toCelsiusScale == a - 273.15
  }

  property("ThermalCapacity * Temperature = Energy") = forAll(posNum, posNum) { (thermCap: TestData, temp: TestData) ⇒

    implicit val tempTol = Kelvin(tol)
    implicit val thermTol = JoulesPerKelvin(tol)
    implicit val energyTol = Joules(1e-12)

    Joules(thermCap * temp) =~ JoulesPerKelvin(thermCap) * Kelvin(temp) &&
      Joules(thermCap * temp) =~ Kelvin(temp) * JoulesPerKelvin(thermCap) &&
      Kelvin(temp) =~ Joules(thermCap * temp) / JoulesPerKelvin(thermCap) &&
      JoulesPerKelvin(thermCap) =~ Joules(thermCap * temp) / Kelvin(temp)
  }
}
