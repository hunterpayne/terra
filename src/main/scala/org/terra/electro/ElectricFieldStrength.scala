
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
final class ElectricFieldStrengthLike[C <: TypeContext](
  val value: C#T, val unit: ElectricFieldStrengthUnit[C])(
  implicit ops: TerraOps[C])
  extends Quantity[ElectricFieldStrengthLike[C], C#T, C] {

  import ops.electricFieldStrengthOps._
  import ops.electricPotentialOps.Volts

  type Length = LengthLike[C]
  type ElectricPotential = ElectricPotentialLike[C]

  def dimension: Dimension[ElectricFieldStrengthLike[C], C#T, C] = 
    ElectricFieldStrength

  def *(that: Length)(implicit ops: TerraOps[C]): ElectricPotential =
    Volts(ops.num.times(this.toVoltsPerMeter, that.toMeters))

  def toVoltsPerMeter = to(VoltsPerMeter)
}

trait ElectricFieldStrengthUnit[C <: TypeContext] 
    extends UnitOfMeasure[ElectricFieldStrengthLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) =
    new ElectricFieldStrengthLike[C](t, this)
}

trait ElectricFieldStrengthOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]
  //def convDouble(d: Double)(implicit n: Numeric[C#T]): C#T

  trait ElectricFieldStrengthUnitT extends ElectricFieldStrengthUnit[C] {
    //def fromDouble(d: Double)(implicit n: Numeric[T]): T = convDouble(d)
  }

  object ElectricFieldStrength 
      extends Dimension[ElectricFieldStrengthLike[C], C#T, C] {
    private[electro] def apply[A](a: A, unit: ElectricFieldStrengthUnit[C])(
      implicit n: Numeric[A]) =
      new ElectricFieldStrengthLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "ElectricFieldStrength"
    def primaryUnit = VoltsPerMeter
    def siUnit = VoltsPerMeter
    def units = Set(VoltsPerMeter)
  }

  import ops.electricPotentialOps.Volts

  object VoltsPerMeter extends ElectricFieldStrengthUnitT 
      with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = Volts.symbol + "/" + ops.lengthOps.Meters.symbol
  }

  object ElectricFieldStrengthConversions {
    lazy val voltPerMeter = VoltsPerMeter(1)

    implicit class ElectricFieldStrengthConversions[A](a: A)(
      implicit num: Numeric[A]) {
      def voltsPerMeter = VoltsPerMeter(a)
    }
  }
}

