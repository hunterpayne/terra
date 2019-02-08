
package org.terra
package thermal

/**
  * Contains all the symbols exported by the org.terra.*.thermal packages.
  * Synthetic packages then subclass this trait with an object to represent
  * a package in the synthetic trees of symbols used by the unit test and
  * user's calling code.
  * @author Hunter Payne
  */
trait ThermalSymbols[Tuple <: TypeContext] {

  implicit val ops: TerraOps[Tuple]

  type Temperature = TemperatureLike[Tuple]
  lazy val Kelvin = ops.temperatureOps.Kelvin
  lazy val Fahrenheit = ops.temperatureOps.Fahrenheit
  lazy val Celsius = ops.temperatureOps.Celsius
  lazy val Rankine = ops.temperatureOps.Rankine

  object TemperatureConversions {
    import ops.temperatureOps.{ TemperatureConversions => Convs }

    lazy val kelvin = Convs.kelvin
    lazy val fahrenheit = Convs.fahrenheit
    lazy val celsius = Convs.celsius
    lazy val rankine = Convs.rankine

    def celsiusToFahrenheitDegrees(celsius: Tuple#T) =
      Convs.celsiusToFahrenheitDegrees(celsius)
    def fahrenheitToCelsiusDegrees(fahrenheit: Tuple#T) =
      Convs.fahrenheitToCelsiusDegrees(fahrenheit)
    def celsiusToKelvinDegrees(celsius: Tuple#T) =
      Convs.celsiusToKelvinDegrees(celsius)
    def kelvinToCelsiusDegrees(kelvin: Tuple#T) =
      Convs.kelvinToCelsiusDegrees(kelvin)
    def fahrenheitToKelvinDegrees(fahrenheit: Tuple#T) =
      Convs.fahrenheitToKelvinDegrees(fahrenheit)
    def kelvinToFahrenheitDegrees(kelvin: Tuple#T) =
      Convs.kelvinToFahrenheitDegrees(kelvin)
    def celsiusToRankineDegrees(celsius: Tuple#T) =
      Convs.celsiusToRankineDegrees(celsius)
    def rankineToCelsiusDegrees(rankine: Tuple#T) =
      Convs.rankineToCelsiusDegrees(rankine)
    def fahrenheitToRankineDegrees(fahrenheit: Tuple#T) =
      Convs.fahrenheitToRankineDegrees(fahrenheit)
    def rankineToFahrenheitDegrees(rankine: Tuple#T) =
      Convs.rankineToFahrenheitDegrees(rankine)
    def kelvinToRankineDegrees(kelvin: Tuple#T) =
      Convs.kelvinToRankineDegrees(kelvin)
    def rankineToKelvinDegrees(rankine: Tuple#T) =
      Convs.rankineToKelvinDegrees(rankine)

    /*
     * Scale conversions are used to convert a "thermometer" temperature 
     * from one scale to another.
     * These conversions will adjust the result by the zero offset.
     * They are used to find the equivalent absolute temperature in the 
     * other scale.
     */
    def celsiusToFahrenheitScale(celsius: Tuple#T) =
      Convs.celsiusToFahrenheitScale(celsius)
    def fahrenheitToCelsiusScale(fahrenheit: Tuple#T) =
      Convs.fahrenheitToCelsiusScale(fahrenheit)
    def celsiusToKelvinScale(celsius: Tuple#T) =
      Convs.celsiusToKelvinScale(celsius)
    def kelvinToCelsiusScale(kelvin: Tuple#T) =
      Convs.kelvinToCelsiusScale(kelvin)
    def fahrenheitToKelvinScale(fahrenheit: Tuple#T) =
      Convs.fahrenheitToKelvinScale(fahrenheit)
    def kelvinToFahrenheitScale(kelvin: Tuple#T) =
      Convs.kelvinToFahrenheitScale(kelvin)
    def celsiusToRankineScale(celsius: Tuple#T) =
      Convs.celsiusToRankineScale(celsius)
    def rankineToCelsiusScale(rankine: Tuple#T) =
        Convs.rankineToCelsiusScale(rankine)
    def fahrenheitToRankineScale(fahrenheit: Tuple#T) =
      Convs.fahrenheitToRankineScale(fahrenheit)
    def rankineToFahrenheitScale(rankine: Tuple#T) =
      Convs.rankineToFahrenheitScale(rankine)
    def kelvinToRankineScale(kelvin: Tuple#T) =
      Convs.kelvinToRankineScale(kelvin)
    def rankineToKelvinScale(rankine: Tuple#T) =
      Convs.rankineToKelvinScale(rankine)

    implicit class TemperatureConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.TemperatureConversions[A](a)
    implicit class TemperatureStringConversion(s: String)
        extends Convs.TemperatureStringConversion(s)
  }
  val Temperature = ops.temperatureOps.Temperature

  type ThermalCapacity = ThermalCapacityLike[Tuple]
  lazy val JoulesPerKelvin = ops.thermalCapacityOps.JoulesPerKelvin

  object ThermalCapacityConversions {
    import ops.thermalCapacityOps.{ ThermalCapacityConversions => Convs }

    lazy val joulePerKelvin = Convs.joulePerKelvin

    implicit class ThermalCapacityConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.ThermalCapacityConversions[A](a)

    implicit object ThermalCapacityNumeric
        extends AbstractQuantityNumeric[ThermalCapacityLike[Tuple], Tuple](
      ThermalCapacity)
  }
  val ThermalCapacity = ops.thermalCapacityOps.ThermalCapacity

  lazy val AbsoluteZero = Kelvin(0)
  lazy val FreezingTemperatureWater = Celsius(0)
  lazy val BoilingTemperatureWater = Celsius(100)
}
