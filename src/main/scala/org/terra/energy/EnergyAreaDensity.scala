
package org.terra
package energy

import scala.reflect.ClassTag

import space.AreaLike

/**
  *
  */
final class EnergyAreaDensityLike[C <: TypeContext](
  val value: C#T, val unit: EnergyAreaDensityUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[EnergyAreaDensityLike[C], C#T, C] {

  import ops.energyAreaDensityOps._
  import ops.energyOps.Joules

  type Area = AreaLike[C]
  type Energy = EnergyLike[C]

  def dimension: Dimension[EnergyAreaDensityLike[C], C#T, C] = EnergyAreaDensity

  def *(that: Area)(implicit ops: TerraOps[C]): Energy =
    Joules(ops.num.times(this.toJoulesPerSquareMeter, that.toSquareMeters))

  def toJoulesPerSquareMeter = to(JoulesPerSquareMeter)
}

trait EnergyAreaDensityUnit[C <: TypeContext] 
    extends UnitOfMeasure[EnergyAreaDensityLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) =
    new EnergyAreaDensityLike[C](t, this)
}

trait EnergyAreaDensityOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]

  trait EnergyAreaDensityUnitT extends EnergyAreaDensityUnit[C]

  object EnergyAreaDensity extends Dimension[EnergyAreaDensityLike[C], C#T, C] {
    private[energy] def apply[A](a: A, unit: EnergyAreaDensityUnit[C])(
      implicit n: Numeric[A]) =
      new EnergyAreaDensityLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "EnergyAreaDensity"
    def primaryUnit = JoulesPerSquareMeter
    def siUnit = JoulesPerSquareMeter
    def units = Set(JoulesPerSquareMeter)
  }


  object JoulesPerSquareMeter extends EnergyAreaDensityUnitT with PrimaryUnit[C#T, C]
      with SiUnit {
    val symbol = "j/m²"
  }

  object EnergyAreaDensityConversions {
    lazy val joulePerSquareMeter = JoulesPerSquareMeter(1)

    implicit class EnergyAreaDensityConversions[A](a: A)(implicit num: Numeric[A]) {
      def joulesPerSquareMeter = JoulesPerSquareMeter(a)
    }
  }
}

