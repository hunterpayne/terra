/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package electro

import space.LengthLike

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value value in [[org.terra.electro.Farads]]
 */
final class CapacitanceLike[C <: TypeContext](
  val value: C#T, val unit: CapacitanceUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[CapacitanceLike[C], C#T, C] {

  import ops.capacitanceOps._
  import ops.electricChargeOps.Coulombs
  import ops.permittivityOps.FaradsPerMeter

  type ElectricPotential = ElectricPotentialLike[C]
  type ElectricCharge = ElectricChargeLike[C]
  type Length = LengthLike[C]
  type Permittivity = PermittivityLike[C]

  def dimension: Dimension[CapacitanceLike[C], C#T, C] = Capacitance

  def *(that: ElectricPotential)(implicit ops: TerraOps[C]): ElectricCharge =
    Coulombs(num.times(this.toFarads, that.toVolts))
  def /(that: Length)(implicit ops: TerraOps[C]): Permittivity = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    FaradsPerMeter(ops.div[C#T](this.toFarads, that.toMeters))
  }

  def toFarads = to(Farads)
  def toPicofarads = to(Picofarads)
  def toNanofarads = to(Nanofarads)
  def toMicrofarads = to(Microfarads)
  def toMillifarads = to(Millifarads)
  def toKilofarads = to(Kilofarads)
}

trait CapacitanceUnit[C <: TypeContext] 
    extends UnitOfMeasure[CapacitanceLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) = 
    new CapacitanceLike[C](t, this)
}

trait CapacitanceOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]

  trait CapacitanceUnitT extends CapacitanceUnit[C]

  object Capacitance extends Dimension[CapacitanceLike[C], C#T, C] {
    private[electro] def apply[A](a: A, unit: CapacitanceUnit[C])(
      implicit n: Numeric[A]) =
      new CapacitanceLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "Capacitance"
    def primaryUnit = Farads
    def siUnit = Farads
    def units = Set(Farads, Picofarads, Nanofarads, Microfarads, Millifarads, Kilofarads)
  }


  object Farads extends CapacitanceUnitT with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = "F"
  }

  object Picofarads extends CapacitanceUnitT with SiUnit {
    val symbol = "pF"
    val conversionFactor = MetricSystem.Pico
  }

  object Nanofarads extends CapacitanceUnitT with SiUnit {
    val symbol = "nF"
    val conversionFactor = MetricSystem.Nano
  }

  object Microfarads extends CapacitanceUnitT with SiUnit {
    val symbol = "μF"
    val conversionFactor = MetricSystem.Micro
  }

  object Millifarads extends CapacitanceUnitT with SiUnit {
    val symbol = "mF"
    val conversionFactor = MetricSystem.Milli
  }

  object Kilofarads extends CapacitanceUnitT with SiUnit {
    val symbol = "kF"
    val conversionFactor = MetricSystem.Kilo
  }

  object CapacitanceConversions {
    lazy val farad = Farads(1)
    lazy val picofarad = Picofarads(1)
    lazy val nanofarad = Nanofarads(1)
    lazy val microfarad = Microfarads(1)
    lazy val millifarad = Millifarads(1)
    lazy val kilofarad = Kilofarads(1)

    implicit class CapacitanceConversions[A](a: A)(implicit num: Numeric[A]) {
      def farads = Farads(a)
      def picofarads = Picofarads(a)
      def nanofarads = Nanofarads(a)
      def microfarads = Microfarads(a)
      def millifarads = Millifarads(a)
      def kilofarads = Kilofarads(a)
    }
  }
}
