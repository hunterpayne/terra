/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package electro

import scala.reflect.ClassTag

import energy.{ EnergyLike, PowerLike }
import time.TimeDerivative
import space.LengthLike

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value value in [[org.terra.electro.Volts]]
 */
final class ElectricPotentialLike[C <: TypeContext](
  val value: C#T, val unit: ElectricPotentialUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[ElectricPotentialLike[C], C#T, C]
    with TimeDerivative[MagneticFluxLike[C], C#T, C#T, C] {

  import ops.electricPotentialOps._
  import ops.magneticFluxOps.Webers
  import ops.timeOps.Seconds
  import ops.powerOps.Watts
  import ops.electricChargeOps.Coulombs
  import ops.energyOps.Joules
  import ops.electricalResistanceOps.Ohms
  import ops.electricCurrentOps.Amperes
  import ops.electricFieldStrengthOps.VoltsPerMeter

  type Power = PowerLike[C]
  type ElectricCurrent = ElectricCurrentLike[C]
  type Capacitance = CapacitanceLike[C]
  type ElectricCharge = ElectricChargeLike[C]
  type Energy = EnergyLike[C]
  type ElectricalResistance = ElectricalResistanceLike[C]
  type Length = LengthLike[C]
  type ElectricFieldStrength = ElectricFieldStrengthLike[C]

  def dimension: Dimension[ElectricPotentialLike[C], C#T, C] = ElectricPotential

  protected[terra] def timeIntegrated = Webers(toVolts)
  protected[terra] def time = Seconds(1)

  def *(that: ElectricCurrent)(implicit ops: TerraOps[C]): Power =
    Watts(ops.num.times(toVolts, that.toAmperes))
  def *(that: Capacitance)(implicit ops: TerraOps[C]): ElectricCharge =
    Coulombs(ops.num.times(toVolts, that.toFarads))
  def *(that: ElectricCharge)(implicit ops: TerraOps[C]): Energy =
    Joules(ops.num.times(toVolts, that.toCoulombs))
  def /(that: ElectricCurrent)(implicit ops: TerraOps[C]): ElectricalResistance = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    Ohms(ops.div[C#T](this.toVolts, that.toAmperes))
  }
  def /(that: ElectricalResistance)(implicit ops: TerraOps[C]): ElectricCurrent = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    Amperes(ops.div[C#T](this.toVolts, that.toOhms))
  }
  def /(that: Length)(implicit ops: TerraOps[C]): ElectricFieldStrength = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    VoltsPerMeter(ops.div[C#T](this.toVolts, that.toMeters))
  }

  def toVolts = to(Volts)
  def toMicrovolts = to(Microvolts)
  def toMillivolts = to(Millivolts)
  def toKilovolts = to(Kilovolts)
  def toMegavolts = to(Megavolts)
}

trait ElectricPotentialUnit[C <: TypeContext] 
    extends UnitOfMeasure[ElectricPotentialLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) = 
    new ElectricPotentialLike[C](t, this)
}

trait ElectricPotentialOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]

  trait ElectricPotentialUnitT extends ElectricPotentialUnit[C]

  object ElectricPotential extends Dimension[ElectricPotentialLike[C], C#T, C] {
    private[electro] def apply[A](a: A, unit: ElectricPotentialUnit[C])(
      implicit n: Numeric[A]) = 
      new ElectricPotentialLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "ElectricPotential"
    def primaryUnit = Volts
    def siUnit = Volts
    def units = Set(Volts, Microvolts, Millivolts, Kilovolts, Megavolts)
  }


  object Volts extends ElectricPotentialUnitT with PrimaryUnit[C#T, C] 
      with SiUnit {
    val symbol = "V"
  }

  object Microvolts extends ElectricPotentialUnitT with SiUnit {
    val symbol = "μV"
    val conversionFactor = MetricSystem.Micro
  }

  object Millivolts extends ElectricPotentialUnitT with SiUnit {
    val symbol = "mV"
    val conversionFactor = MetricSystem.Milli
  }

  object Kilovolts extends ElectricPotentialUnitT with SiUnit {
    val symbol = "kV"
    val conversionFactor = MetricSystem.Kilo
  }

  object Megavolts extends ElectricPotentialUnitT with SiUnit {
    val symbol = "MV"
    val conversionFactor = MetricSystem.Mega
  }

  object ElectricPotentialConversions {
    lazy val volt = Volts(1)
    lazy val microvolt = Microvolts(1)
    lazy val millivolt = Millivolts(1)
    lazy val kilovolt = Kilovolts(1)
    lazy val megavolt = Megavolts(1)

    implicit class ElectricPotentialConversions[A](a: A)(implicit num: Numeric[A]) {
      def V = Volts(a)
      def volts = Volts(a)
      def microvolts = Microvolts(a)
      def millivolts = Millivolts(a)
      def kilovolts = Kilovolts(a)
      def megavolts = Megavolts(a)
    }
  }
}


