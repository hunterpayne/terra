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

import space.LengthLike

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value value in [[org.terra.electro.OhmMeters]]
 */
final class ResistivityLike[C <: TypeContext](
  val value: C#T, val unit: ResistivityUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[ResistivityLike[C], C#T, C] {

  import ops.resistivityOps._
  import ops.conductivityOps.SiemensPerMeter
  import ops.lengthOps.Meters
  import ops.electricalResistanceOps.Ohms

  type Length = LengthLike[C]
  type ElectricalResistance = ElectricalResistanceLike[C]

  def dimension: Dimension[ResistivityLike[C], C#T, C] = Resistivity

  def /(that: Length)(implicit ops: TerraOps[C]): ElectricalResistance = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    Ohms(ops.div[C#T](this.toOhmMeters, that.toMeters))
  }
  def /(that: ElectricalResistance)(implicit ops: TerraOps[C]): Length = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    Meters(ops.div[C#T](this.toOhmMeters, that.toOhms))
  }

  def toOhmMeters = to(OhmMeters)
  def inSiemensPerMeter = {
    implicit val ensureT: HasEnsureType[C#T] = ops.converters.ensureT
    SiemensPerMeter(ops.div(ops.convDouble(1d), toOhmMeters))
  }
}

trait ResistivityUnit[C <: TypeContext] 
    extends UnitOfMeasure[ResistivityLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) = 
    new ResistivityLike[C](t, this)
}

trait ResistivityOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]

  trait ResistivityUnitT extends ResistivityUnit[C]

  object Resistivity extends Dimension[ResistivityLike[C], C#T, C] {
    private[electro] def apply[A](a: A, unit: ResistivityUnit[C])(
      implicit n: Numeric[A]) =
      new ResistivityLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "Resistivity"
    def primaryUnit = OhmMeters
    def siUnit = OhmMeters
    def units = Set(OhmMeters)
  }


  object OhmMeters extends ResistivityUnitT with PrimaryUnit[C#T, C] 
      with SiUnit {
    def symbol = "Ω⋅m"
  }

  object ResistivityConversions {
    lazy val ohmMeter = OhmMeters(1)

    implicit class ResistivityConversions[A](a: A)(implicit num: Numeric[A]) {
      def ohmMeters = OhmMeters(a)
    }
  }
}

