/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package thermal

import scala.util.{ Failure, Success, Try }
import scala.reflect.ClassTag

/**
 * Represents a quantity of temperature
 *
 * Temperatures are somewhat unique in the world of quantities for a couple of reasons.
 *
 * First, different units (scales) have different "zero" values.  This means that these scales
 * are not simple multiples of the others.  There is a "zero offset" that must be applied to conversions
 * from one scale to another.
 *
 * Second, temperatures are often quoted as though they were quantities, when in fact they are just points
 * on a scale.  Similar to a mile marker on a highway, the quantity represented is the number degrees (miles)
 * from a specific "zero" value on the scale.
 *
 * In fact an absolute quantity of thermodynamic temperature should be measured from absolute zero.
 * Thus, Kelvin, is the SI Base unit for temperature.
 *
 * The other scales supported here, Celsius and Fahrenheit, are known as empirical scales.
 * Of course, these scales set their respective zero values well above absolute zero.
 * This is done to provide a granular and reasonably sized ranges of values for dealing with everyday temperatures.
 *
 * This library supports another absolute scale, the Rankine scale. Rankine sets its zero at absolute zero,
 * but degrees are measure in Fahrenheit (as opposed to Celsius, as the Kelvin scale uses).
 *
 * In consideration of these more unique scale conversions, two conversion types are supported: Degrees and Scale.
 *
 * Scale based conversions DO adjust for the zero offset.
 * Thus 5 degrees C is the same as 41 degrees F on the thermometer.
 *
 * Degrees based conversions DO NOT adjust for the zero point.
 * Thus 5 degrees C|K is the same amount of temperature as 9 degrees F|R.
 *
 * When creating a temperature it is not important to consider these differences.
 * It is also irrelevant when performing operation on temperatures in the same scale.
 * However, when performing operations on two temperatures of different scales these factors do become important.
 *
 * The Quantity.to(unit) and Quantity.in(unit) methods are overridden to use Scale conversions for convenience
 *
 * The Ordered.compare method is implemented to use Scale conversions
 *
 * The Quantity.plus and Quantity.minus methods are implemented to treat right operands as Quantity of Degrees and not a scale Temperature.
 * Operands that differ in scale will use Degree conversions.
 * This supports mixed scale expressions:
 *
 * val temp = Fahrenheit(100) - Celsius(5) // returns Fahrenheit(91)
 *
 * This also supports declaring temperature ranges using typical nomenclature:
 *
 * val tempRange = 65.F +- 5.C // returns QuantityRange(56.0°F,74.0°F)
 *
 * The toDegrees(unit) methods are implemented to use Degree conversions.
 *
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value the value of the temperature
 */

final class TemperatureLike[C <: TypeContext](
  val value: C#T, val unit: TemperatureScale[C])(
  implicit ops: TerraOps[C])
    extends Quantity[TemperatureLike[C], C#T, C] {

  import ops.temperatureOps._
  import ops.energyOps.Joules
  
  type ThermalCapacity = ThermalCapacityLike[C]
  type Temperature = TemperatureLike[C]

  def dimension: Dimension[TemperatureLike[C], C#T, C] = Temperature

  override def plus(that: Temperature)(implicit ops: TerraOps[C]): Temperature =
    new TemperatureLike[C](ops.num.plus(
      this.value, that.convert(unit, withOffset = false).value), unit)
  override def minus(that: Temperature)(implicit ops: TerraOps[C]): Temperature = 
    new TemperatureLike[C](ops.num.minus(
      this.value, that.convert(unit, withOffset = false).value), unit)

  def *(that: ThermalCapacity) = 
    Joules(ops.num.times(this.toKelvinScale, that.toJoulesPerKelvin))

  override def toString: String = unit match {
    case Kelvin => super.toString
    case _ => {
      implicit val num = ops.num
      crossFormat(value) + unit.symbol // Non-Kelvin units are treated in a special manner, they do not get a space between the value and symbol.
    }
  }

  def toString(unit: TemperatureScale[C]): String = in(unit).toString

  private def convert(toScale: TemperatureScale[C], withOffset: Boolean = true)(
    implicit ops: TerraOps[C]): Temperature = 
    (unit, toScale, withOffset) match {
      case (Fahrenheit, Fahrenheit, _)  ⇒ this
      case (Celsius, Celsius, _)        ⇒ this
      case (Kelvin, Kelvin, _)          ⇒ this
      case (Rankine, Rankine, _)        ⇒ this

      case (Fahrenheit, Celsius, true)  ⇒ Celsius(TemperatureConversions.fahrenheitToCelsiusScale(value))
      case (Celsius, Fahrenheit, true)  ⇒ Fahrenheit(TemperatureConversions.celsiusToFahrenheitScale(value))
      case (Celsius, Kelvin, true)      ⇒ Kelvin(TemperatureConversions.celsiusToKelvinScale(value))
      case (Kelvin, Celsius, true)      ⇒ Celsius(TemperatureConversions.kelvinToCelsiusScale(value))
      case (Fahrenheit, Kelvin, true)   ⇒ Kelvin(TemperatureConversions.fahrenheitToKelvinScale(value))
      case (Kelvin, Fahrenheit, true)   ⇒ Fahrenheit(TemperatureConversions.kelvinToFahrenheitScale(value))
      case (Fahrenheit, Rankine, true)  ⇒ Rankine(TemperatureConversions.fahrenheitToRankineScale(value))
      case (Rankine, Fahrenheit, true)  ⇒ Fahrenheit(TemperatureConversions.rankineToFahrenheitScale(value))
      case (Celsius, Rankine, true)     ⇒ Rankine(TemperatureConversions.celsiusToRankineScale(value))
      case (Rankine, Celsius, true)     ⇒ Celsius(TemperatureConversions.rankineToCelsiusScale(value))
      case (Kelvin, Rankine, true)      ⇒ Rankine(TemperatureConversions.kelvinToRankineScale(value))
      case (Rankine, Kelvin, true)      ⇒ Kelvin(TemperatureConversions.rankineToKelvinScale(value))


      case (Fahrenheit, Celsius, false) ⇒ Celsius(TemperatureConversions.fahrenheitToCelsiusDegrees(value))
      case (Celsius, Fahrenheit, false) ⇒ Fahrenheit(TemperatureConversions.celsiusToFahrenheitDegrees(value))
      case (Celsius, Kelvin, false)     ⇒ Kelvin(TemperatureConversions.celsiusToKelvinDegrees(value))
      case (Kelvin, Celsius, false)     ⇒ Celsius(TemperatureConversions.kelvinToCelsiusDegrees(value))
      case (Fahrenheit, Kelvin, false)  ⇒ Kelvin(TemperatureConversions.fahrenheitToKelvinDegrees(value))
      case (Kelvin, Fahrenheit, false)  ⇒ Fahrenheit(TemperatureConversions.kelvinToFahrenheitDegrees(value))
      case (Fahrenheit, Rankine, false) ⇒ Rankine(TemperatureConversions.fahrenheitToRankineDegrees(value))
      case (Rankine, Fahrenheit, false) ⇒ Fahrenheit(TemperatureConversions.rankineToFahrenheitDegrees(value))
      case (Celsius, Rankine, false)    ⇒ Rankine(TemperatureConversions.celsiusToRankineDegrees(value))
      case (Rankine, Celsius, false)    ⇒ Celsius(TemperatureConversions.rankineToCelsiusDegrees(value))
      case (Kelvin, Rankine, false)     ⇒ Rankine(TemperatureConversions.kelvinToRankineDegrees(value))
      case (Rankine, Kelvin, false)     ⇒ Kelvin(TemperatureConversions.rankineToKelvinDegrees(value))
    }

  def in(unit: TemperatureScale[C]) = convert(unit, withOffset = true)
  def inFahrenheit(implicit ops: TerraOps[C]): Temperature = convert(Fahrenheit)
  def inCelsius(implicit ops: TerraOps[C]): Temperature = convert(Celsius)
  def inKelvin(implicit ops: TerraOps[C]): Temperature = convert(Kelvin)

  def to(unit: TemperatureScale[C]) = toScale(unit)
  def toScale(unit: TemperatureScale[C]) = 
    convert(unit, withOffset = true).value
  def toFahrenheitScale = toScale(Fahrenheit)
  def toCelsiusScale = toScale(Celsius)
  def toKelvinScale = toScale(Kelvin)

  def toDegrees(unit: TemperatureScale[C]) = 
    convert(unit, withOffset = false).value
  def toFahrenheitDegrees = toDegrees(Fahrenheit)
  def toCelsiusDegrees = toDegrees(Celsius)
  def toKelvinDegrees = toDegrees(Kelvin)
}

/**
 * Base trait for units of [[org.terra.thermal.Temperature]]
 */
sealed trait TemperatureScale[C <: TypeContext] 
    extends UnitOfMeasure[TemperatureLike[C], C#T, C] {
  def self: TemperatureScale[C]
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) =
    new TemperatureLike(t, this)
}

trait TemperatureOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]

  trait TemperatureUnitT extends TemperatureScale[C]

  /**
    * Temperature companion object
    */
  object Temperature extends Dimension[TemperatureLike[C], C#T, C] 
      with BaseDimension {
    def apply[A](a: A, scale: TemperatureScale[C])(implicit n: Numeric[A]) = 
      new TemperatureLike[C](ops.convDouble(n.toDouble(a)), scale)

    def apply(s: String): Try[TemperatureLike[C]] = {
      val regex = "([-+]?[0-9]*\\.?[0-9]+(?:[eE][-+]?[0-9]+)?) *°? *(f|F|c|C|k|K|r|R)".r
      s match {
        case regex(value, unit) => unit match {
          case "f" | "F" => Success(Fahrenheit(value.toDouble))
          case "c" | "C" => Success(Celsius(value.toDouble))
          case "k" | "K" => Success(Kelvin(value.toDouble))
          case "r" | "R" => Success(Rankine(value.toDouble))
        }
        case _ => Failure(QuantityParseException("Unable to parse Temperature", s))
      }
    }

    def name = "Temperature"
    def primaryUnit = Kelvin
    def siUnit = Kelvin
    def units = Set(Kelvin, Fahrenheit, Celsius, Rankine)
    def dimensionSymbol = "Θ"
  }

  object Celsius extends TemperatureUnitT {
    val symbol = "°C"
    val self = this
    protected def converterFrom(implicit ops: TerraOps[C]) = 
      TemperatureConversions.celsiusToKelvinScale(_)
    protected def converterTo(implicit ops: TerraOps[C]) = 
      TemperatureConversions.kelvinToCelsiusScale(_)
    def apply(temperature: TemperatureLike[C]): TemperatureLike[C] =
      temperature.inCelsius
  }

  object Fahrenheit extends TemperatureUnitT {
    val symbol = "°F"
    val self = this
    protected def converterFrom(implicit ops: TerraOps[C]) = 
      TemperatureConversions.fahrenheitToKelvinScale(_)
    protected def converterTo(implicit ops: TerraOps[C]) = 
      TemperatureConversions.kelvinToFahrenheitScale(_)
    def apply(temperature: TemperatureLike[C]): TemperatureLike[C] = 
      temperature.inFahrenheit
  }

  object Kelvin extends TemperatureUnitT with PrimaryUnit[C#T, C] 
      with SiBaseUnit {
    val symbol = "K"
    val self = this
    def apply(temperature: TemperatureLike[C]): TemperatureLike[C] =
      temperature.inKelvin
  }

  object Rankine extends TemperatureUnitT {
    val symbol = "°R"
    val self = this
    protected def converterFrom(implicit ops: TerraOps[C]) = 
      TemperatureConversions.rankineToKelvinScale(_)
    protected def converterTo(implicit ops: TerraOps[C]) = 
      TemperatureConversions.kelvinToRankineScale(_)
    def apply(temperature: TemperatureLike[C]): TemperatureLike[C] = 
      temperature.in(Rankine)
  }

  object TemperatureConversions {

    lazy val kelvin = Kelvin(1)
    lazy val fahrenheit = Fahrenheit(1)
    lazy val celsius = Celsius(1)
    lazy val rankine = Rankine(1)

    /*
     * Degree conversions are used to convert a quantity of degrees from one scale to another.
     * These conversions do not adjust for the zero offset.
     * Essentially they only do the 9:5 conversion between F degrees and C|K degrees
     */
    def celsiusToFahrenheitDegrees(celsius: C#T) = {
      implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
      implicit val tag: ClassTag[C#T] = ops.getClassTagT
      ops.div[C#T](
        ops.num.times(celsius, ops.convDouble(9d)), ops.convDouble(5d))
    }
    def fahrenheitToCelsiusDegrees(fahrenheit: C#T) = {
      implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
      implicit val tag: ClassTag[C#T] = ops.getClassTagT
      ops.div[C#T](
        ops.num.times(fahrenheit, ops.convDouble(5d)), ops.convDouble(9d))
    }
    def celsiusToKelvinDegrees(celsius: C#T) = celsius
    def kelvinToCelsiusDegrees(kelvin: C#T) = kelvin
    def fahrenheitToKelvinDegrees(fahrenheit: C#T) = {
      implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
      implicit val tag: ClassTag[C#T] = ops.getClassTagT
      ops.div[C#T](
        ops.num.times(fahrenheit, ops.convDouble(5d)), ops.convDouble(9d))
    }
    def kelvinToFahrenheitDegrees(kelvin: C#T) = {
      implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
      implicit val tag: ClassTag[C#T] = ops.getClassTagT
      ops.div[C#T](
        ops.num.times(kelvin, ops.convDouble(9d)), ops.convDouble(5d))
    }
    def celsiusToRankineDegrees(celsius: C#T) = {
      implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
      implicit val tag: ClassTag[C#T] = ops.getClassTagT
      ops.div[C#T](
        ops.num.times(celsius, ops.convDouble(9d)), ops.convDouble(5d))
    }
    def rankineToCelsiusDegrees(rankine: C#T) = {
      implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
      implicit val tag: ClassTag[C#T] = ops.getClassTagT
      ops.div[C#T](
        ops.num.times(rankine, ops.convDouble(5d)), ops.convDouble(9d))
    }
    def fahrenheitToRankineDegrees(fahrenheit: C#T) = fahrenheit
    def rankineToFahrenheitDegrees(rankine: C#T) = rankine
    def kelvinToRankineDegrees(kelvin: C#T) = {
      implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
      implicit val tag: ClassTag[C#T] = ops.getClassTagT
      ops.div[C#T](
        ops.num.times(kelvin, ops.convDouble(9d)), ops.convDouble(5d))
    }
    def rankineToKelvinDegrees(rankine: C#T) = {
      implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
      implicit val tag: ClassTag[C#T] = ops.getClassTagT
      ops.div[C#T](
        ops.num.times(rankine, ops.convDouble(5d)), ops.convDouble(9d))
    }

    /*
     * Scale conversions are used to convert a "thermometer" temperature from one scale to another.
     * These conversions will adjust the result by the zero offset.
     * They are used to find the equivalent absolute temperature in the other scale.
     */
    def celsiusToFahrenheitScale(celsius: C#T) = {
      implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
      implicit val tag: ClassTag[C#T] = ops.getClassTagT
      ops.num.plus(
        ops.div[C#T](
          ops.num.times(celsius, ops.convDouble(9d)), 
          ops.convDouble(5d)),
        ops.convDouble(32d))
    }
    def fahrenheitToCelsiusScale(fahrenheit: C#T) = {
      implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
      implicit val tag: ClassTag[C#T] = ops.getClassTagT
      ops.div[C#T](ops.num.times(
        ops.num.minus(fahrenheit, ops.convDouble(32d)),
        ops.convDouble(5d)), ops.convDouble(9d))
    }
    def celsiusToKelvinScale(celsius: C#T) = 
      ops.num.plus(celsius, ops.convDouble(273.15))
    def kelvinToCelsiusScale(kelvin: C#T) = 
      ops.num.minus(kelvin, ops.convDouble(273.15))
    def fahrenheitToKelvinScale(fahrenheit: C#T) = {
      implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
      implicit val tag: ClassTag[C#T] = ops.getClassTagT
      ops.div[C#T](
        ops.num.times(
          ops.num.plus(fahrenheit, ops.convDouble(459.67d)),
          ops.convDouble(5d)),
        ops.convDouble(9d))
    }
    def kelvinToFahrenheitScale(kelvin: C#T) = {
      implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
      implicit val tag: ClassTag[C#T] = ops.getClassTagT
      ops.num.minus(
        ops.div[C#T](
          ops.num.times(kelvin, ops.convDouble(9d)), ops.convDouble(5d)),
        ops.convDouble(459.67))
    }
    def celsiusToRankineScale(celsius: C#T) = {
      implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
      implicit val tag: ClassTag[C#T] = ops.getClassTagT
      ops.div[C#T](
        ops.num.times(
          ops.num.plus(celsius, ops.convDouble(273.15)), ops.convDouble(9d)),
        ops.convDouble(5d))
    }
    def rankineToCelsiusScale(rankine: C#T) = {
      implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
      implicit val tag: ClassTag[C#T] = ops.getClassTagT
      ops.div[C#T](
        ops.num.times(
          ops.num.minus(rankine, ops.convDouble(491.67)), 
          ops.convDouble(5d)), 
        ops.convDouble(9d))
    }
    def fahrenheitToRankineScale(fahrenheit: C#T) = 
      ops.num.plus(fahrenheit, ops.convDouble(459.67))
    def rankineToFahrenheitScale(rankine: C#T) = 
      ops.num.minus(rankine, ops.convDouble(459.67))
    def kelvinToRankineScale(kelvin: C#T) = {
      implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
      implicit val tag: ClassTag[C#T] = ops.getClassTagT
      ops.div[C#T](
        ops.num.times(kelvin, ops.convDouble(9d)), ops.convDouble(5d))
    }
    def rankineToKelvinScale(rankine: C#T) = {
      implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
      implicit val tag: ClassTag[C#T] = ops.getClassTagT
      ops.div[C#T](
        ops.num.times(rankine, ops.convDouble(5d)), ops.convDouble(9d))
    }

    implicit class TemperatureConversions[A](a: A)(implicit n: Numeric[A]) {
      def C = Celsius(a)
      def celsius = Celsius(a)
      def degreesCelsius = Celsius(a)
      def F = Fahrenheit(a)
      def Fah = Fahrenheit(a) // F conflicts with (Float) in the console; Fah is provided as an alternative
      def fahrenheit = Fahrenheit(a)
      def degreesFahrenheit = Fahrenheit(a)
      def K = Kelvin(a)
      def kelvin = Kelvin(a)
      def degreesKelvin = Kelvin(a)
      def R = Rankine(a)
      def rankine = Rankine(a)
      def degreesRankine = Rankine(a)
    }

    implicit class TemperatureStringConversion(s: String) {
      def toTemperature = Temperature(s)
    }
  }
}


