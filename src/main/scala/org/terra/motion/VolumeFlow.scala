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

import time._
import space.VolumeLike

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value Double
 */
final class VolumeFlowLike[C <: TypeContext](val value: C#T, val unit: VolumeFlowRateUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[VolumeFlowLike[C], C#T, C]
    with TimeDerivative[VolumeLike[C], C#T, C#T, C] {

  import ops.volumeFlowOps._
  import ops.timeOps.Seconds
  import ops.volumeOps.CubicMeters

  def dimension: Dimension[VolumeFlowLike[C], C#T, C] = VolumeFlow

  protected[terra] def timeIntegrated = CubicMeters(toCubicMetersPerSecond)
  protected[terra] def time = Seconds(1)

  def toCubicMetersPerSecond = to(CubicMetersPerSecond)
  def toCubicFeetPerHour = to(CubicFeetPerHour)
  def toGallonsPerDay = to(GallonsPerDay)
  def toGallonsPerHour = to(GallonsPerHour)
  def toGallonsPerMinute = to(GallonsPerMinute)
  def toGallonsPerSecond = to(GallonsPerSecond)
}

trait VolumeFlowRateUnit[C <: TypeContext] 
    extends UnitOfMeasure[VolumeFlowLike[C], C#T, C]
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) = 
    new VolumeFlowLike[C](t, this)
}

trait VolumeFlowOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]
  def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait VolumeFlowRateUnitT extends VolumeFlowRateUnit[C]

  object VolumeFlow extends Dimension[VolumeFlowLike[C], C#T, C] {
    private[motion] def apply[A](a: A, unit: VolumeFlowRateUnit[C])(
      implicit n: Numeric[A]) =
      new VolumeFlowLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "VolumeFlow"
    def primaryUnit = CubicMetersPerSecond
    def siUnit = CubicMetersPerSecond
    def units = Set(CubicMetersPerSecond, CubicFeetPerHour, GallonsPerDay, GallonsPerHour, GallonsPerMinute, GallonsPerSecond)
  }

  import ops.volumeOps.{ CubicFeet, CubicMeters, UsGallons }

  object CubicMetersPerSecond extends VolumeFlowRateUnitT 
      with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = "m³/s"
  }

  object CubicFeetPerHour extends VolumeFlowRateUnitT {
    val symbol = "ft³/hr"
    val conversionFactor = 
      (CubicFeet.conversionFactor / CubicMeters.conversionFactor) / 
        ops.timeOps.Time.SecondsPerHour
  }

  object GallonsPerDay extends VolumeFlowRateUnitT {
    val symbol = "GPD"
    val conversionFactor = 
      (UsGallons.conversionFactor / CubicMeters.conversionFactor) / 
        ops.timeOps.Time.SecondsPerDay
  }

  object GallonsPerHour extends VolumeFlowRateUnitT {
    val symbol = "GPH"
    val conversionFactor = 
      (UsGallons.conversionFactor / CubicMeters.conversionFactor) / 
        ops.timeOps.Time.SecondsPerHour
  }

  object GallonsPerMinute extends VolumeFlowRateUnitT {
    val symbol = "GPM"
    val conversionFactor = 
      (UsGallons.conversionFactor / CubicMeters.conversionFactor) / 
        ops.timeOps.Time.SecondsPerMinute
  }

  object GallonsPerSecond extends VolumeFlowRateUnitT {
    val symbol = "GPS"
    val conversionFactor = 
      UsGallons.conversionFactor / CubicMeters.conversionFactor
  }

  object VolumeFlowConversions {
    lazy val cubicMeterPerSecond = CubicMetersPerSecond(1)
    lazy val cubicFootPerHour = CubicFeetPerHour(1)
    lazy val gallonPerDay = GallonsPerDay(1)
    lazy val gallonPerHour = GallonsPerHour(1)
    lazy val gallonPerMinute = GallonsPerMinute(1)
    lazy val gallonPerSecond = GallonsPerSecond(1)

    implicit class VolumeFlowConversions[A](a: A)(implicit n: Numeric[A]) {
      def cubicMetersPerSecond = CubicMetersPerSecond(a)
      def cubicFeetPerHour = CubicFeetPerHour(a)
      def gallonsPerDay = GallonsPerDay(a)
      def gallonsPerHour = GallonsPerHour(a)
      def gallonsPerMinute = GallonsPerMinute(a)
      def gallonsPerSecond = GallonsPerSecond(a)
    }
  }
}

