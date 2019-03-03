/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package electro

import energy.PowerLike
import time.TimeDerivative
import space.{ LengthLike, AreaLike }

/**
 * Represents a quantity of electric current
 *
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value the amount of charge in [[org.terra.electro.Amperes]]'s
 */
final class ElectricCurrentLike[C <: TypeContext](
  val value: C#T, val unit: ElectricCurrentUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[ElectricCurrentLike[C], C#T, C]
    with TimeDerivative[ElectricChargeLike[C], C#T, C#T, C] {

  import ops.electricCurrentOps._
  import ops.electricChargeOps.Coulombs
  import ops.timeOps.Seconds
  import ops.electricPotentialOps.Volts
  import ops.powerOps.Watts
  import ops.magneticFluxOps.Webers
  import ops.electricalConductanceOps.Siemens
  import ops.magneticFieldStrengthOps.AmperesPerMeter
  import ops.electricCurrentDensityOps.AmperesPerSquareMeter

  type ElectricalResistance = ElectricalResistanceLike[C]
  type ElectricPotential = ElectricPotentialLike[C]
  type Power = PowerLike[C]
  type Inductance = InductanceLike[C]
  type MagneticFlux = MagneticFluxLike[C]
  type ElectricalConductance = ElectricalConductanceLike[C]
  type Length = LengthLike[C]
  type MagneticFieldStrength = MagneticFieldStrengthLike[C]
  type Area = AreaLike[C]
  type ElectricCurrentDensity = ElectricCurrentDensityLike[C]

  def dimension: Dimension[ElectricCurrentLike[C], C#T, C] = ElectricCurrent

  protected[terra] def timeIntegrated = Coulombs(toAmperes)
  protected[terra] def time = Seconds(1)

  def *(that: ElectricalResistance)(
    implicit ops: TerraOps[C]): ElectricPotential = 
    Volts(ops.num.times(this.toAmperes, that.toOhms))
  def *(that: ElectricPotential)(implicit ops: TerraOps[C]): Power = 
    Watts(ops.num.times(this.toAmperes, that.toVolts))
  def *(that: Inductance)(implicit ops: TerraOps[C]): MagneticFlux = 
    Webers(ops.num.times(this.toAmperes, that.toHenry))
  def /(that: ElectricPotential)(
    implicit ops: TerraOps[C]): ElectricalConductance = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    Siemens(ops.div[C#T](this.toAmperes, that.toVolts))
  }
  def /(that: Length)(implicit ops: TerraOps[C]): MagneticFieldStrength = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    AmperesPerMeter(ops.div[C#T](this.toAmperes, that.toMeters))
  }
  def /(that: Area)(implicit ops: TerraOps[C]): ElectricCurrentDensity = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    AmperesPerSquareMeter(ops.div[C#T](this.toAmperes, that.toSquareMeters))
  }

  def toAmperes = to(Amperes)
  def toMilliamperes = to(Milliamperes)
}

/**
 * Base trait for units of [[org.terra.electro.ElectricCurrentLike]]
 */
trait ElectricCurrentUnit[C <: TypeContext] 
    extends UnitOfMeasure[ElectricCurrentLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) =
    new ElectricCurrentLike[C](t, this)
}

trait ElectricCurrentOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]

  trait ElectricCurrentUnitT extends ElectricCurrentUnit[C]

  object ElectricCurrent extends Dimension[ElectricCurrentLike[C], C#T, C]
      with BaseDimension {
    private[electro] def apply[A](a: A, unit: ElectricCurrentUnit[C])(
      implicit n: Numeric[A]) = 
      new ElectricCurrentLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "ElectricCurrent"
    def primaryUnit = Amperes
    def siUnit = Amperes
    def units = Set(Amperes, Milliamperes)
    def dimensionSymbol = "I"
  }


  /**
    * Amperes
    */
  object Amperes extends ElectricCurrentUnitT with PrimaryUnit[C#T, C] 
      with SiBaseUnit {
    val symbol = "A"
  }

  /**
    * Milliamperes
    */
  object Milliamperes extends ElectricCurrentUnitT with SiUnit {
    val symbol = "mA"
    val conversionFactor = MetricSystem.Milli
  }

  object ElectricCurrentConversions {
    lazy val ampere = Amperes(1)
    lazy val amp = Amperes(1)
    lazy val milliampere = Milliamperes(1)
    lazy val milliamp = Milliamperes(1)

    implicit class ElectricCurrentConversions[A](a: A)(implicit num: Numeric[A]) {
      def amperes = Amperes(a)
      def amps = Amperes(a)
      def A = Amperes(a)
      def milliampers = Milliamperes(a)
      def milliamps = Milliamperes(a)
      def mA = Milliamperes(a)
    }
  }
}

