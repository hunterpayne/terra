/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package motion

import space.AreaLike
import time.{ TimeIntegral, TimeDerivative, TimeLike }

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value Double
 */
final class PressureLike[C <: TypeContext](val value: C#T, val unit: PressureUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[PressureLike[C], C#T, C]
    with TimeDerivative[ViscosityLike[C], C#T, C#T, C]
    with TimeIntegral[PressureChangeLike[C], C#T, C#T, C] {

  import ops.pressureOps._
  import ops.pressureChangeOps.PascalsPerSecond
  import ops.timeOps.Seconds
  import ops.forceOps.Newtons
  import ops.viscosityOps.PascalSeconds

  type PressureChange = PressureChangeLike[C]
  type Area = AreaLike[C]
  type Force = ForceLike[C]
  type Viscosity = ViscosityLike[C]

  def dimension: Dimension[PressureLike[C], C#T, C] = Pressure

  override protected def timeDerived: PressureChange = {
    implicit val opsArg = ops
    PascalsPerSecond(toPascals)
  }
  override protected[terra] def time: Time = {
    implicit val opsArg = ops
    Seconds(1)
  }
  protected[terra] def timeIntegrated: Viscosity = {
    implicit val opsArg = ops
    PascalSeconds(toPascals)
  }

  def *(that: Area)(implicit ops: TerraOps[C]): Force = 
    Newtons(ops.num.times(this.toPascals, that.toSquareMeters))

  def toPascals              = to(Pascals)
  def toBars                 = to(Bars)
  def toPoundsPerSquareInch  = to(PoundsPerSquareInch)
  def toStandardAtmospheres  = to(StandardAtmospheres)
  def toMillimetersOfMercury = to(MillimetersOfMercury)
  def toInchesOfMercury      = to(InchesOfMercury)
  def toTorr                 = to(Torrs)
}

trait PressureUnit[C <: TypeContext] 
    extends UnitOfMeasure[PressureLike[C], C#T, C] with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) = new PressureLike[C](t, this)
}

trait PressureOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]
  //def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait PressureUnitT extends PressureUnit[C]

  object Pressure extends Dimension[PressureLike[C], C#T, C] {
    private[motion] def apply[A](a: A, unit: PressureUnit[C])(
      implicit n: Numeric[A]) =
      new PressureLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "Pressure"
    def primaryUnit = Pascals
    def siUnit = Pascals
    def units = Set(Pascals, Bars, PoundsPerSquareInch, StandardAtmospheres, MillimetersOfMercury, InchesOfMercury, Torrs)
  }


  object Pascals extends PressureUnitT with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = "Pa"
  }

  object Bars extends PressureUnitT {
    val symbol = "bar"
    val conversionFactor = 100000d
  }

  import ops.forceOps.{ Newtons, PoundForce }
  import ops.areaOps.{ SquareInches, SquareMeters }

  object PoundsPerSquareInch extends PressureUnitT {
    val symbol = "psi"
    val conversionFactor = 
      (PoundForce.conversionFactor / Newtons.conversionFactor) / 
        (SquareInches.conversionFactor / SquareMeters.conversionFactor)
  }

  object StandardAtmospheres extends PressureUnitT {
    val symbol = "atm"
    val conversionFactor = Newtons.conversionFactor * 1.01325e5
  }

  object MillimetersOfMercury extends PressureUnitT {
    val symbol = "mmHg"
    val conversionFactor = Newtons.conversionFactor * 133.322387415
  }

  object InchesOfMercury extends PressureUnitT {
    val symbol = "inHg"
    val conversionFactor = Newtons.conversionFactor * 3386.389
  }

  object Torrs extends PressureUnitT {
    val symbol = "Torr"
    val conversionFactor = StandardAtmospheres.conversionFactor / 760d
  }

  object PressureConversions {
    lazy val pascal = Pascals(1)
    lazy val bar    = Bars(1)
    lazy val psi    = PoundsPerSquareInch(1)
    lazy val atm    = StandardAtmospheres(1)
    lazy val mmHg   = MillimetersOfMercury(1)
    lazy val inHg   = InchesOfMercury(1)
    lazy val torr   = Torrs(1)

    implicit class PressureConversions[A](a: A)(implicit n: Numeric[A]) {
      def pascals = Pascals(a)
      def bars    = Bars(a)
      def psi     = PoundsPerSquareInch(a)
      def atm     = StandardAtmospheres(a)
      def mmHg    = MillimetersOfMercury(a)
      def inHg    = InchesOfMercury(a)
      def torr    = Torrs(a)
    }
  }
}

