/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package motion

import org.terra.time.TimeDerivative

/**
 * @author  stevebarham
 * @since   0.5.2
 *
 * @param value Double
 */
final class PressureChangeLike[C <: TypeContext](
  val value: C#T, val unit: PressureChangeUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[PressureChangeLike[C], C#T, C]
    with TimeDerivative[PressureLike[C], C#T, C#T, C] {

  import ops.pressureChangeOps._
  import ops.pressureOps.Pascals
  import ops.timeOps.Seconds

  type Pressure = PressureLike[C]

  def dimension: Dimension[PressureChangeLike[C], C#T, C] = PressureChange

  protected[terra] def timeIntegrated = Pascals(toPascalsPerSecond)
  protected[terra] def time = Seconds(1)

  def toPascalsPerSecond = to(PascalsPerSecond)
  def toBarsPerSecond = to(BarsPerSecond)
  def toPoundsPerSquareInchPerSecond = to(PoundsPerSquareInchPerSecond)
  def toStandardAtmospheresPerSecond = to(StandardAtmospheresPerSecond)
}

trait PressureChangeUnit[C <: TypeContext] 
    extends UnitOfMeasure[PressureChangeLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) = 
    new PressureChangeLike[C](t, this)
}

trait PressureChangeOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]
  //  def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait PressureChangeUnitT extends PressureChangeUnit[C]

  object PressureChange extends Dimension[PressureChangeLike[C], C#T, C] {
    private[motion] def apply[A](a: A, unit: PressureChangeUnit[C])(
      implicit n: Numeric[A]) = 
      new PressureChangeLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "PressureChange"
    def primaryUnit = PascalsPerSecond
    def siUnit = PascalsPerSecond
    def units = Set(PascalsPerSecond, BarsPerSecond, PoundsPerSquareInchPerSecond, StandardAtmospheresPerSecond)
  }

  object PascalsPerSecond extends PressureChangeUnitT with PrimaryUnit[C#T, C]
      with SiUnit {
    val symbol = "Pa/s"
  }

  import ops.pressureOps.{ Bars, Pascals, StandardAtmospheres, PoundsPerSquareInch }

  object BarsPerSecond extends PressureChangeUnitT {
    val symbol = "bar/s"
    val conversionFactor = Bars.conversionFactor / Pascals.conversionFactor
  }

  object PoundsPerSquareInchPerSecond extends PressureChangeUnitT {
    val symbol = "psi/s"
    val conversionFactor = PoundsPerSquareInch.conversionFactor
  }

  object StandardAtmospheresPerSecond extends PressureChangeUnitT {
    val symbol = "atm/s"
    val conversionFactor = StandardAtmospheres.conversionFactor
  }

  object PressureChangeConversions {
    lazy val pascalPerSecond = PascalsPerSecond(1)
    lazy val barPerSecond = BarsPerSecond(1)
    lazy val poundPerSquareInchPerSecond = PoundsPerSquareInchPerSecond(1)
    lazy val standardAtmospherePerSecond = StandardAtmospheresPerSecond(1)

    implicit class PressureChangeConversions[A](a: A)(implicit n: Numeric[A]) {
      def pascalsPerSecond = PascalsPerSecond(a)
      def barsPerSecond = BarsPerSecond(a)
      def poundsPerSquareInchPerSecond = PoundsPerSquareInchPerSecond(a)
      def standardAtmospheresPerSecond = StandardAtmospheresPerSecond(a)
    }
  }
}

