
package org.terra
package space

import scala.reflect.ClassTag

import time.TimeLike
import mass.{ MassLike, DensityLike }

/**
  *
  */
final class SpecificVolumeLike[C <: TypeContext](
  val value: C#T, val unit: SpecificVolumeUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[SpecificVolumeLike[C], C#T, C] {

  import ops.specificVolumeOps._
  import ops.energyOps.Joules
  import ops.massOps.Mass
  import ops.timeOps.Time
  import ops.volumeOps.CubicMeters
  import ops.densityOps.KilogramsPerCubicMeter

  type Mass = MassLike[C]
  type Volume = VolumeLike[C]
  type Density = DensityLike[C]

  def dimension: Dimension[SpecificVolumeLike[C], C#T, C] = SpecificVolume

  def *(that: Mass)(implicit ops: TerraOps[C]): Volume = 
    CubicMeters(ops.num.times(this.toCubicMetersPerKilogram, that.toKilograms))

  def inv(implicit ops: TerraOps[C]): Density = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    KilogramsPerCubicMeter(ops.div[C#T](
      ops.convDouble(1.0), toCubicMetersPerKilogram))
  }

  def toCubicMetersPerKilogram = to(CubicMetersPerKilogram)
  def toMillilitresPerGram = to(MillilitresPerGram)
  def toCubicFeetPerPound = to(CubicFeetPerPound)
  def toCubicFeetPerSlug = to(CubicFeetPerSlug)
}

trait SpecificVolumeUnit[C <: TypeContext] 
    extends UnitOfMeasure[SpecificVolumeLike[C], C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) = 
    new SpecificVolumeLike[C](t, this)
}

trait SpecificVolumeOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]

  trait SpecificVolumeUnitT extends SpecificVolumeUnit[C]

  object SpecificVolume extends Dimension[SpecificVolumeLike[C], C#T, C] {
    private[space] def apply[A](a: A, unit: SpecificVolumeUnit[C])(
      implicit n: Numeric[A]) = 
      new SpecificVolumeLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "SpecificVolume"
    def primaryUnit = CubicMetersPerKilogram
    def siUnit = CubicMetersPerKilogram
    def units = Set(
      CubicMetersPerKilogram, MillilitresPerGram, 
      CubicFeetPerPound, CubicFeetPerSlug)
  }

  import ops.volumeOps.{ CubicMeters, Millilitres, CubicFeet }
  import ops.massOps.{ Kilograms, Grams, Pounds }
  import ops.massOps.Slugs

  object CubicMetersPerKilogram extends SpecificVolumeUnitT 
      with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = CubicMeters.symbol + "/" + Kilograms.symbol
  }

  object MillilitresPerGram extends SpecificVolumeUnitT 
      with UnitConverter[C#T, C] {
    val symbol = Millilitres.symbol + "/" + Grams.symbol
    val conversionFactor = 1.0
  }

  object CubicFeetPerPound extends SpecificVolumeUnitT 
      with UnitConverter[C#T, C] {
    val symbol = CubicFeet.symbol + "/" + Pounds.symbol
    val conversionFactor =
      (CubicFeet.conversionFactor / Pounds.conversionFactor) * MetricSystem.Kilo
  }

  object CubicFeetPerSlug extends SpecificVolumeUnitT
      with UnitConverter[C#T, C] {
    val symbol = CubicFeet.symbol + "/" + Slugs.symbol
    val conversionFactor = CubicFeet.conversionFactor / Slugs.conversionFactor
  }

  object SpecificVolumeConversions {
    lazy val cubicMeterPerKilogram = CubicMetersPerKilogram(1)
    lazy val millilitrePerGram = MillilitresPerGram(1)
    lazy val cubicFootPerPound = CubicFeetPerPound(1)
    lazy val cubicFootPerSlug = CubicFeetPerSlug(1)

    implicit class SpecificVolumeConversions[A](a: A)(implicit num: Numeric[A]) {
      def cubicMetersPerKilogram = CubicMetersPerKilogram(a)
      def millilitresPerGram = MillilitresPerGram(a)
      def cubicFeetPerPound = CubicFeetPerPound(a)
      def cubicFeetPerSlug = CubicFeetPerSlug(a)
    }
  }
}

