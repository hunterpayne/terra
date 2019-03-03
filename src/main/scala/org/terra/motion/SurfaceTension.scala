
package org.terra
package motion

import org.terra.space.LengthLike
import org.terra.time.{ SecondTimeDerivative, TimeDerivative, TimeSquaredLike }

/**
  *
  */
final class SurfaceTensionLike[C <: TypeContext](
  val value: C#T, val unit: SurfaceTensionUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[SurfaceTensionLike[C], C#T, C] {

  import ops.surfaceTensionOps._
  import ops.forceOps.Newtons

  type Length = LengthLike[C]
  type Force = ForceLike[C]

  def dimension: Dimension[SurfaceTensionLike[C], C#T, C] = SurfaceTension

  def *(that: Length)(implicit ops: TerraOps[C]): Force =
    Newtons(ops.num.times(this.toNewtonsPerMeter, that.toMeters))

  def toNewtonsPerMeter = to(NewtonsPerMeter)
  def toPoundsPerFoot = to(PoundsPerFoot)
}

trait SurfaceTensionUnit[C <: TypeContext] 
    extends UnitOfMeasure[SurfaceTensionLike[C], C#T, C]
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) =
    new SurfaceTensionLike[C](t, this)
}

trait SurfaceTensionOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]
  //def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait SurfaceTensionUnitT extends SurfaceTensionUnit[C]

  object SurfaceTension extends Dimension[SurfaceTensionLike[C], C#T, C] {
    private[motion] def apply[A](a: A, unit: SurfaceTensionUnit[C])(
      implicit n: Numeric[A]) =
      new SurfaceTensionLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "SurfaceTension"
    def primaryUnit = NewtonsPerMeter
    def siUnit = NewtonsPerMeter
    def units = Set(NewtonsPerMeter, PoundsPerFoot)
  }

  import ops.forceOps.{ Newtons, PoundForce }
  import ops.lengthOps.{ Meters, Feet }
  import ops.massOps.Pounds

  object NewtonsPerMeter extends SurfaceTensionUnitT with PrimaryUnit[C#T, C]
      with SiUnit {
    val symbol = Newtons.symbol + "/" + Meters.symbol
  }

  object PoundsPerFoot extends SurfaceTensionUnitT with UnitConverter[C#T, C] {
    val symbol = Pounds.symbol + "/" + Feet.symbol
    val conversionFactor = PoundForce.conversionFactor / Feet.conversionFactor
  }

  object SurfaceTensionConversions {
    lazy val newtonPerMeter = NewtonsPerMeter(1)
    lazy val poundPerFoot = PoundsPerFoot(1)

    implicit class SurfaceTensionConversions[A](a: A)(implicit n: Numeric[A]) {
      def newtonsPerMeter = NewtonsPerMeter(a)
      def poundsPerFoot = PoundsPerFoot(a)
    }
  }
}
