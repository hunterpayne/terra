
package org.terra.classic

import org.scalacheck.Prop._
import org.scalacheck.Properties

import mass._
import space.{ CubicMeters, SquareMeters }

/**
 *
 */
object MassChecks extends Properties("Mass") with QuantityChecks {

  property("Mass = Density * Volume") = forAll(posNum, posNum) { (density: TestData, volume: TestData) ⇒
    Kilograms(density * volume) == KilogramsPerCubicMeter(density) * CubicMeters(volume) &&
      Kilograms(density * volume) == CubicMeters(volume) * KilogramsPerCubicMeter(density) &&
      KilogramsPerCubicMeter(density) == Kilograms(density * volume) / CubicMeters(volume) &&
      CubicMeters(volume) == Kilograms(density * volume) / KilogramsPerCubicMeter(density)
  }

  property("Mass = AreaDensity * Area") = forAll(posNum, posNum) { (density: TestData, area: TestData) ⇒
    Kilograms(density * area) == KilogramsPerSquareMeter(density) * SquareMeters(area) &&
      Kilograms(density * area) == SquareMeters(area) * KilogramsPerSquareMeter(density) &&
      KilogramsPerSquareMeter(density) == Kilograms(density * area) / SquareMeters(area) &&
      SquareMeters(area) == Kilograms(density * area) / KilogramsPerSquareMeter(density)
  }
}
