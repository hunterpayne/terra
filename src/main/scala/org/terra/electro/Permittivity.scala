
package org.terra
package electro

import scala.reflect.ClassTag

import space.LengthLike

/**
  *
  * @author Nicolas Vinuesa
  * @since 1.4
  *
  * @param value Double
  */
final class PermittivityLike[C <: TypeContext](
  val value: C#T, val unit: PermittivityUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[PermittivityLike[C], C#T, C] {

  import ops.permittivityOps._
  import ops.capacitanceOps.Farads

  type Length = LengthLike[C]
  type Capacitance = CapacitanceLike[C]

  def dimension: Dimension[PermittivityLike[C], C#T, C] = Permittivity

  def *(that: Length)(implicit ops: TerraOps[C]): Capacitance =
    Farads(ops.num.times(this.toFaradsMeters, that.toMeters))

  def toFaradsMeters = to(FaradsPerMeter)
}

trait PermittivityUnit[C <: TypeContext] 
    extends UnitOfMeasure[PermittivityLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) = 
    new PermittivityLike[C](t, this)
}

trait PermittivityOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]

  trait PermittivityUnitT extends PermittivityUnit[C]

  object Permittivity extends Dimension[PermittivityLike[C], C#T, C] {
    private[electro] def apply[A](a: A, unit: PermittivityUnit[C])(
      implicit n: Numeric[A]) = 
      new PermittivityLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "Permittivity"
    def primaryUnit = FaradsPerMeter
    def siUnit = FaradsPerMeter
    def units = Set(FaradsPerMeter)
  }

  import ops.lengthOps.Meters
  import ops.capacitanceOps.Farads

  object FaradsPerMeter extends PermittivityUnitT with PrimaryUnit[C#T, C]
      with SiUnit {
    val symbol = Farads.symbol + "/" + Meters.symbol
  }

  object PermittivityConversions {
    lazy val faradPerMeter = FaradsPerMeter(1)

    implicit class PermittivityConversions[A](a: A)(implicit num: Numeric[A]) {
      def faradsPerMeter = FaradsPerMeter(a)
    }
  }
}

