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
 * @param value value in [[org.terra.standard.electro.Siemens]]
 */
final class ElectricalConductanceLike[C <: TypeContext](
  val value: C#T, val unit: ElectricalConductanceUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[ElectricalConductanceLike[C], C#T, C] {

  import ops.electricalConductanceOps._
  import ops.electricalResistanceOps.Ohms
  import ops.lengthOps.Meters
  import ops.conductivityOps.SiemensPerMeter

  type Length = LengthLike[C]
  type Conductivity = ConductivityLike[C]

  def dimension: Dimension[ElectricalConductanceLike[C], C#T, C] = 
    ElectricalConductance

  def /(that: Length)(implicit ops: TerraOps[C]): Conductivity = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    SiemensPerMeter(ops.div[C#T](this.toSiemens, that.toMeters))
  }
  def /(that: Conductivity)(implicit ops: TerraOps[C]): Length = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    Meters(ops.div[C#T](this.toSiemens, that.toSiemensPerMeter))
  }

  def toSiemens = to(Siemens)

  def inOhms = {
    implicit val ensureT: HasEnsureType[C#T] = ops.converters.ensureT
    Ohms(ops.div[C#T](ops.num.fromInt(1), value))
  }
}

trait ElectricalConductanceUnit[C <: TypeContext] 
    extends UnitOfMeasure[ElectricalConductanceLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) =
    new ElectricalConductanceLike[C](t, this)
}

trait ElectricalConductanceOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]

  trait ElectricalConductanceUnitT extends ElectricalConductanceUnit[C]

  object ElectricalConductance 
      extends Dimension[ElectricalConductanceLike[C], C#T, C] {
    private[electro] def apply[A](a: A, unit: ElectricalConductanceUnit[C])(
      implicit n: Numeric[A]) = 
      new ElectricalConductanceLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "ElectricalConductance"
    def primaryUnit = Siemens
    def siUnit = Siemens
    def units = Set(Siemens)
  }


  object Siemens extends ElectricalConductanceUnitT with PrimaryUnit[C#T, C] 
      with SiUnit {
    val symbol = "S"
}

  object ElectricalConductanceConversions {
    lazy val siemen = Siemens(1)

    implicit class ElectricalConductanceConversions[A](a: A)(
      implicit num: Numeric[A]) {
      def siemens = Siemens(a)
    }
  }
}

