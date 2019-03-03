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
 * @param value value in [[org.terra.electro.SiemensPerMeter]]
 */
final class ConductivityLike[C <: TypeContext](
  val value: C#T, val unit: ConductivityUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[ConductivityLike[C], C#T, C] {

  import ops.conductivityOps._
  import ops.resistivityOps.OhmMeters
  import ops.electricalConductanceOps.Siemens

  type Length = LengthLike[C]
  type ElectricalConductance = ElectricalConductanceLike[C]

  def dimension: Dimension[ConductivityLike[C], C#T, C] = Conductivity

  def *(that: Length)(implicit ops: TerraOps[C]): ElectricalConductance = 
    Siemens(ops.num.times(this.toSiemensPerMeter, that.toMeters))

  def toSiemensPerMeter = to(SiemensPerMeter)
  def inOhmMeters = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    OhmMeters(ops.div[C#T](ops.num.fromInt(1), toSiemensPerMeter))
  }
}

trait ConductivityUnit[C <: TypeContext] 
    extends UnitOfMeasure[ConductivityLike[C], C#T, C]
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) = 
    new ConductivityLike[C](t, this)
}

trait ConductivityOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]

  trait ConductivityUnitT extends ConductivityUnit[C]

  object Conductivity extends Dimension[ConductivityLike[C], C#T, C] {
    private[electro] def apply[A](a: A, unit: ConductivityUnit[C])(
      implicit n: Numeric[A]) = 
      new ConductivityLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "Conductivity"
    def primaryUnit = SiemensPerMeter
    def siUnit = SiemensPerMeter
    def units = Set(SiemensPerMeter)
  }

  object SiemensPerMeter extends ConductivityUnitT with PrimaryUnit[C#T, C] 
      with SiUnit {
    val symbol = "S/m"
  }

  object ConductivityConversions {
    lazy val siemenPerMeter = SiemensPerMeter(1)

    implicit class ConductivityConversions[A](a: A)(implicit num: Numeric[A]) {
      def siemensPerMeter = SiemensPerMeter(a)
    }
  }
}
