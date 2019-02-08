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

import space.{ LengthLike, AreaLike }
import org.terra.time.{ TimeDerivative, TimeIntegral }
import org.terra.energy.EnergyLike
import org.terra.mass.MassLike

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value Double
 */
final class ForceLike[C <: TypeContext](val value: C#T, val unit: ForceUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[ForceLike[C], C#T, C]
    with TimeDerivative[MomentumLike[C], C#T, C#T, C] 
    with TimeIntegral[YankLike[C], C#T, C#T, C] {

  import ops.forceOps._
  import ops.timeOps.Seconds
  import ops.momentumOps.NewtonSeconds
  import ops.yankOps.NewtonsPerSecond
  import ops.energyOps.Joules
  import ops.accelerationOps.MetersPerSecondSquared
  import ops.massOps.Kilograms
  import ops.pressureOps.Pascals
  import ops.areaOps.SquareMeters

  type Torque = TorqueLike[C]
  type Length = LengthLike[C]
  type Energy = EnergyLike[C]
  type Mass = MassLike[C]
  type Acceleration = AccelerationLike[C]
  type Pressure = PressureLike[C]
  type Area = AreaLike[C]

  def dimension: Dimension[ForceLike[C], C#T, C] = Force

  protected[terra] def timeIntegrated = NewtonSeconds(toNewtons)
  protected def timeDerived = NewtonsPerSecond(toNewtons)
  override def time = Seconds(1)

  /* This could also be Torque, as Energy(Work) and Torque are
   * dimensionally equivalent */
  def *(that: Length)(implicit ops: TerraOps[C]): Energy = 
    Joules(ops.num.times(this.toNewtons, that.toMeters))
  def /(that: Length)(implicit ops: TerraOps[C]) = ??? // return SurfaceTension
  def /(that: Mass)(implicit ops: TerraOps[C]): Acceleration = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    MetersPerSecondSquared(ops.div[C#T](this.toNewtons, that.toKilograms))
  }
  def /(that: Acceleration)(implicit ops: TerraOps[C]): Mass = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    Kilograms(ops.div[C#T](this.toNewtons, that.toMetersPerSecondSquared))
  }
  def /(that: Area)(implicit ops: TerraOps[C]): Pressure = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    Pascals(ops.div[C#T](this.toNewtons, that.toSquareMeters))
  }
  def /(that: Pressure)(implicit ops: TerraOps[C]): Area = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    SquareMeters(ops.div[C#T](this.toNewtons, that.toPascals))
  }

  def toNewtons = to(Newtons)
  def toKilogramForce = to(KilogramForce)
  def toPoundForce = to(PoundForce)
  def toKiloElectronVoltsPerMicrometer = to(KiloElectronVoltsPerMicrometer)
  def toMegaElectronVoltsPerCentimeter = to(MegaElectronVoltsPerCentimeter)
}

trait ForceUnit[C <: TypeContext] extends UnitOfMeasure[ForceLike[C], C#T, C]
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) = 
    new ForceLike[C](t, this)
}

trait ForceOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]
  def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait ForceUnitT extends ForceUnit[C]

  object Force extends Dimension[ForceLike[C], C#T, C] {
    private[motion] def apply[A](a: A, unit: ForceUnit[C])(
      implicit n: Numeric[A]) = 
      new ForceLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "Force"
    def primaryUnit = Newtons
    def siUnit = Newtons
    def units = Set(
      Newtons, KilogramForce, PoundForce,
      KiloElectronVoltsPerMicrometer, MegaElectronVoltsPerCentimeter)
  }

  import ops.accelerationOps.{ MetersPerSecondSquared, EarthGravities }
  import ops.massOps.{ Pounds, Kilograms }

  object Newtons extends ForceUnitT with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = "N"
  }

  object KilogramForce extends ForceUnitT {
    val symbol = "kgf"
    val conversionFactor = 
      MetersPerSecondSquared.conversionFactor * EarthGravities.conversionFactor
  }

  object PoundForce extends ForceUnitT {
    val symbol = "lbf"
    val conversionFactor = 
      Pounds.conversionFactor *
        KilogramForce.conversionFactor / Kilograms.conversionFactor
  }

  object KiloElectronVoltsPerMicrometer extends ForceUnitT {
    val symbol = "keV/μm"
    val conversionFactor = 1.602176565e-16 / MetricSystem.Micro
  }

  object MegaElectronVoltsPerCentimeter extends ForceUnitT {
    val symbol = "MeV/cm"
    val conversionFactor = 1.602176565e-13 / MetricSystem.Centi
  }

  object ForceConversions {
    lazy val newton = Newtons(1)
    lazy val kilogramForce = KilogramForce(1)
    lazy val poundForce = PoundForce(1)
    lazy val kiloElectronVoltPerMicrometer = KiloElectronVoltsPerMicrometer(1)
    lazy val megaElectronVoltPerCentimeter = MegaElectronVoltsPerCentimeter(1)

    implicit class ForceConversions[A](a: A)(implicit n: Numeric[A]) {
      def newtons = Newtons(a)
      def kilogramForce = KilogramForce(a)
      def poundForce = PoundForce(a)
      def lbf = PoundForce(a)
      def kiloElectronVoltPerMicrometer = KiloElectronVoltsPerMicrometer(a)
      def megaElectronVoltPerCentimeter = MegaElectronVoltsPerCentimeter(a)
    }
  }
}


