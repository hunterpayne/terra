/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package time

import scala.reflect.ClassTag

import electro._
import energy._
import information.{ InformationLike, DataRateLike }
import motion._
import photo._
import space._
import mass.MassLike

/**
  * Represents a quantity of frequency, which is the number cycles (count) over time
  *
  * @author  garyKeorkunian
  * @since   0.1
  *
  * @param value Double
  */
case class FrequencyLike[C <: TypeContext](
  val value: C#T, val unit: FrequencyUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[FrequencyLike[C], C#T, C]
    with TimeDerivative[DimensionlessLike[C], C#T, C#T, C] {

  import ops.frequencyOps._
  import ops.timeOps.Seconds
  import ops.dimensionlessOps.Each
  import ops.converters._

  type Acceleration = AccelerationLike[C]
  type Angle = AngleLike[C]
  type Dimensionless = DimensionlessLike[C]
  type ElectricCharge = ElectricChargeLike[C]
  type Energy = EnergyLike[C]
  type Force = ForceLike[C]
  type Information = InformationLike[C]
  type Length = LengthLike[C]
  type LuminousEnergy = LuminousEnergyLike[C]
  type LuminousExposure = LuminousExposureLike[C]
  type MagneticFlux = MagneticFluxLike[C]
  type Mass = MassLike[C]
  type MassFlow = MassFlowLike[C]
  type Momentum = MomentumLike[C]
  type Power = PowerLike[C]
  type PowerRamp = PowerRampLike[C]
  type Pressure = PressureLike[C]
  type PressureChange = PressureChangeLike[C]
  type Velocity = VelocityLike[C]
  type Volume = VolumeLike[C]
  type VolumeFlow = VolumeFlowLike[C]
  type ElectricPotential = ElectricPotentialLike[C]
  type Illuminance = IlluminanceLike[C]
  type LuminousFlux = LuminousFluxLike[C]
  type DataRate = DataRateLike[C]
  type Yank = YankLike[C]
  type ElectricCurrent = ElectricCurrentLike[C]
  type Frequency = FrequencyLike[C]
  type AngularVelocity = AngularVelocityLike[C]
  type Jerk = JerkLike[C]

  def dimension: Dimension[FrequencyLike[C], C#T, C] = Frequency

  protected[terra] def timeIntegrated = Each(toHertz)
  protected[terra] def time = Seconds(1)

  def *(that: Acceleration)(implicit ops: TerraOps[C]): Jerk = that * this
  def *(that: Angle)(implicit ops: TerraOps[C]): AngularVelocity = that * this
  def *(that: Dimensionless)(implicit ops: TerraOps[C]): Frequency = 
    this * that.toEach
  def *(that: ElectricCharge)(implicit ops: TerraOps[C]): ElectricCurrent = 
    that * this
  def *(that: Energy)(implicit ops: TerraOps[C]): Power = that * this
  def *(that: Force)(implicit ops: TerraOps[C]): Yank = that * this
  def *(that: Information)(implicit ops: TerraOps[C]): DataRate =
    that * this
  def *(that: Length)(implicit ops: TerraOps[C]): Velocity = that * this
  def *(that: LuminousEnergy)(implicit ops: TerraOps[C]): LuminousFlux = 
    that * this
  def *(that: LuminousExposure)(implicit ops: TerraOps[C]): Illuminance = 
    that * this
  def *(that: MagneticFlux)(implicit ops: TerraOps[C]): ElectricPotential = 
    that * this
  def *(that: Mass)(implicit ops: TerraOps[C]): MassFlow = that * this
  def *(that: Momentum)(implicit ops: TerraOps[C]): Force = that * this
  def *(that: Power)(implicit ops: TerraOps[C]): PowerRamp = that * this
  def *(that: Pressure)(implicit ops: TerraOps[C]): PressureChange = that * this
  def *(that: Velocity)(implicit ops: TerraOps[C]): Acceleration = that * this
  def *(that: Volume)(implicit ops: TerraOps[C]): VolumeFlow = that * this

  def toHertz = to(Hertz)
  def toKilohertz = to(Kilohertz)
  def toMegahertz = to(Megahertz)
  def toGigahertz = to(Gigahertz)
  def toTerahertz = to(Terahertz)
  def toRevolutionsPerMinute = to(RevolutionsPerMinute)
}

trait FrequencyUnit[C <: TypeContext] 
    extends UnitOfMeasure[FrequencyLike[C], C#T, C] with UnitConverter[C#T, C] {
  def apply(t: C#T)(
    implicit tag: ClassTag[C#T], ops: TerraOps[C]): FrequencyLike[C] =
    new FrequencyLike[C](t, this)
}

trait FrequencyOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]
  def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait FrequencyUnitT extends FrequencyUnit[C]

  object Frequency extends Dimension[FrequencyLike[C], C#T, C] {
    private[time] def apply[A](a: A, unit: FrequencyUnit[C])(
      implicit num: Numeric[A]) = 
      new FrequencyLike[C](ops.convDouble(num.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "Frequency"
    def primaryUnit = Hertz
    def siUnit = Hertz
    def units = Set(Hertz, Kilohertz, Megahertz, Gigahertz, Terahertz, RevolutionsPerMinute)
  }

  object Hertz extends FrequencyUnitT with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = "Hz"
  }

  object Kilohertz extends FrequencyUnitT with SiUnit {
    val conversionFactor = MetricSystem.Kilo
    val symbol = "kHz"
  }

  object Megahertz extends FrequencyUnitT with SiUnit {
    val conversionFactor = MetricSystem.Mega
    val symbol = "MHz"
  }

  object Gigahertz extends FrequencyUnitT with SiUnit {
    val conversionFactor = MetricSystem.Giga
    val symbol = "GHz"
  }

  object Terahertz extends FrequencyUnitT with SiUnit {
    val conversionFactor = MetricSystem.Tera
    val symbol = "THz"
  }

  object RevolutionsPerMinute extends FrequencyUnitT {
    val conversionFactor = 1d / 60d
    val symbol = "rpm"
  }

  object FrequencyConversions {
    implicit class FrequencyConversions[A](a: A)(implicit num: Numeric[A]) {
      def hertz = Hertz(a)
      def kilohertz = Kilohertz(a)
      def megahertz = Megahertz(a)
      def gigahertz = Gigahertz(a)
      def terahertz = Terahertz(a)
      def rpm = RevolutionsPerMinute(a)
    }

  }
}
