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
 * @param value value in [[org.terra.electro.Ohms]]
 */
final class ElectricalResistanceLike[C <: TypeContext](
  val value: C#T, val unit: ElectricalResistanceUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[ElectricalResistanceLike[C], C#T, C] {

  import ops.electricalResistanceOps._
  import ops.electricPotentialOps.Volts
  import ops.resistivityOps.OhmMeters
  import ops.electricalConductanceOps.Siemens

  type ElectricCurrent = ElectricCurrentLike[C]
  type ElectricPotential = ElectricPotentialLike[C]
  type Length = LengthLike[C]
  type Resistivity = ResistivityLike[C]

  def dimension: Dimension[ElectricalResistanceLike[C], C#T, C] = 
    ElectricalResistance

  def *(that: ElectricCurrent)(implicit ops: TerraOps[C]): ElectricPotential =
    Volts(ops.num.times(this.toOhms, that.toAmperes))
  def *(that: Length)(implicit ops: TerraOps[C]): Resistivity = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    OhmMeters(ops.num.times(this.toOhms, that.toMeters))
  }

  def toOhms = to(Ohms)
  def toNanohms = to(Nanohms)
  def toMicrohms = to(Microohms)
  def toMillohms = to(Milliohms)
  def toKilohms = to(Kilohms)
  def toMegohms = to(Megohms)
  def toGigohms = to(Gigohms)

  def inSiemens = {
    implicit val ensureT: HasEnsureType[C#T] = ops.converters.ensureT
    Siemens(ops.div[C#T](ops.num.fromInt(1), to(Ohms)))
  }
}

trait ElectricalResistanceUnit[C <: TypeContext] 
    extends UnitOfMeasure[ElectricalResistanceLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) = 
    new ElectricalResistanceLike[C](t, this)
}

trait ElectricalResistanceOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]

  trait ElectricalResistanceUnitT extends ElectricalResistanceUnit[C]

  object ElectricalResistance 
      extends Dimension[ElectricalResistanceLike[C], C#T, C] {
    private[electro] def apply[A](a: A, unit: ElectricalResistanceUnit[C])(
      implicit n: Numeric[A]) = 
      new ElectricalResistanceLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "ElectricalResistance"
    def primaryUnit = Ohms
    def siUnit = Ohms
    def units = Set(Ohms, Nanohms, Microohms, Milliohms, Kilohms, Megohms, Gigohms)
  }

  object Ohms extends ElectricalResistanceUnitT with PrimaryUnit[C#T, C] 
      with SiUnit {
    val symbol = "Ω"
}

  object Nanohms extends ElectricalResistanceUnitT with SiUnit {
    val symbol = "nΩ"
    val conversionFactor = MetricSystem.Nano
  }

  object Microohms extends ElectricalResistanceUnitT with SiUnit {
    val symbol = "µΩ"
    val conversionFactor = MetricSystem.Micro
  }

  object Milliohms extends ElectricalResistanceUnitT with SiUnit {
    val symbol = "mΩ"
    val conversionFactor = MetricSystem.Milli
  }

  object Kilohms extends ElectricalResistanceUnitT with SiUnit {
    val symbol = "kΩ"
    val conversionFactor = MetricSystem.Kilo
  }

  object Megohms extends ElectricalResistanceUnitT with SiUnit {
    val symbol = "MΩ"
    val conversionFactor = MetricSystem.Mega
  }

  object Gigohms extends ElectricalResistanceUnitT with SiUnit {
    val symbol = "GΩ"
    val conversionFactor = MetricSystem.Giga
  }

  object ElectricalResistanceConversions {
    lazy val ohm = Ohms(1)
    lazy val nanohm = Nanohms(1)
    lazy val microohm = Microohms(1)
    lazy val milliohm = Milliohms(1)
    lazy val kilohm = Kilohms(1)
    lazy val megohm = Megohms(1)
    lazy val gigohm = Gigohms(1)

    implicit class ElectricalResistanceConversions[A](a: A)(implicit num: Numeric[A]) {
      def ohms = Ohms(a)
      def nanohms = Nanohms(a)
      def microohms = Microohms(a)
      def milliohms = Milliohms(a)
      def kilohms = Kilohms(a)
      def megohms = Megohms(a)
      def gigohms = Gigohms(a)
    }
  }
}

