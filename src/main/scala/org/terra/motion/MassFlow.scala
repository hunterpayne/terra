/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package motion

import scala.reflect.ClassTag

import org.terra.time.{ TimeLike, TimeDerivative }
import org.terra.mass.MassLike

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value Double
 */
final class MassFlowLike[C <: TypeContext](val value: C#T, val unit: MassFlowUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[MassFlowLike[C], C#T, C]
    with TimeDerivative[MassLike[C], C#T, C#T, C] {

  import ops.massFlowOps._
  import ops.timeOps.Seconds
  import ops.massOps.Kilograms  

  def dimension: Dimension[MassFlowLike[C], C#T, C] = MassFlow

  protected[terra] def timeIntegrated = Kilograms(toKilogramsPerSecond)
  protected[terra] def time = Seconds(1)

  def toKilogramsPerSecond = to(KilogramsPerSecond)
  def toPoundsPerSecond = to(PoundsPerSecond)
  def toPoundsPerHour = to(PoundsPerHour)
  def toKilopoundsPerHour = to(KilopoundsPerHour)
  def toMegapoundsPerHour = to(MegapoundsPerHour)
}

trait MassFlowUnit[C <: TypeContext] 
    extends UnitOfMeasure[MassFlowLike[C], C#T, C] with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) = 
    new MassFlowLike[C](t, this)
}

trait MassFlowOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]
  def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait MassFlowUnitT extends MassFlowUnit[C]

  object MassFlow extends Dimension[MassFlowLike[C], C#T, C] {
    private[motion] def apply[A](a: A, unit: MassFlowUnit[C])(
      implicit n: Numeric[A]) =
      new MassFlowLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "MassFlow"
    def primaryUnit = KilogramsPerSecond
    def siUnit = KilogramsPerSecond
    def units = Set(KilogramsPerSecond, PoundsPerSecond, PoundsPerHour, KilopoundsPerHour, MegapoundsPerHour)
  }


  object KilogramsPerSecond extends MassFlowUnitT with PrimaryUnit[C#T, C]
      with SiUnit {
    val symbol = "kg/s"
  }

  object PoundsPerSecond extends MassFlowUnitT {
    val symbol = "lb/s"
    val conversionFactor = 
      ops.massOps.Pounds.conversionFactor / 
        ops.massOps.Kilograms.conversionFactor
  }

  object PoundsPerHour extends MassFlowUnitT {
    val symbol = "lb/hr"
    val conversionFactor = 
      PoundsPerSecond.conversionFactor / ops.timeOps.Time.SecondsPerHour
  }

  object KilopoundsPerHour extends MassFlowUnitT {
    val symbol = "klb/hr"
    val conversionFactor = PoundsPerHour.conversionFactor * MetricSystem.Kilo
  }

  object MegapoundsPerHour extends MassFlowUnitT {
    val symbol = "Mlb/hr"
    val conversionFactor = PoundsPerHour.conversionFactor * MetricSystem.Mega
  }

  object MassFlowConversions {
    lazy val kilogramPerSecond = KilogramsPerSecond(1)
    lazy val poundPerSecond = PoundsPerSecond(1)
    lazy val poundPerHour = PoundsPerHour(1)
    lazy val kilopoundPerHour = KilopoundsPerHour(1)
    lazy val megapoundPerHour = MegapoundsPerHour(1)

    implicit class MassFlowConversions[A](a: A)(implicit n: Numeric[A]) {
      def kilogramsPerSecond = KilogramsPerSecond(a)
      def poundsPerSecond = PoundsPerSecond(a)
      def poundsPerHour = PoundsPerHour(a)
      def kilopoundsPerHour = KilopoundsPerHour(a)
      def megapoundsPerHour = MegapoundsPerHour(a)
    }
  }
}

