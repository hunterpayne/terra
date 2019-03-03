
package org.terra
package radio

import time.TimeLike
import mass.MassLike
import energy.EnergyLike

/**
  *
  */
final class AbsorbedDoseLike[C <: TypeContext](
  val value: C#T, val unit: AbsorbedDoseUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[AbsorbedDoseLike[C], C#T, C] {

  import ops.absorbedDoseOps._
  import ops.energyOps.Joules
  import ops.massOps.Mass
  import ops.timeOps.Time

  type Mass = MassLike[C]
  type Energy = EnergyLike[C]
  type Time = TimeLike[C]

  def dimension: Dimension[AbsorbedDoseLike[C], C#T, C] = AbsorbedDose

  def *(that: Mass)(implicit ops: TerraOps[C]): Energy =
    Joules(ops.num.times(this.toGrays, that.toKilograms))

  def toGrays = to(Grays)
  def toRads = to(Rads)
  def toErgsPerGram = to(ErgsPerGram)
}

trait AbsorbedDoseUnit[C <: TypeContext] 
    extends UnitOfMeasure[AbsorbedDoseLike[C], C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) = 
    new AbsorbedDoseLike[C](t, this)
}

trait AbsorbedDoseOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]

  trait AbsorbedDoseUnitT extends AbsorbedDoseUnit[C]

  object AbsorbedDose extends Dimension[AbsorbedDoseLike[C], C#T, C] {
    private[radio] def apply[A](a: A, unit: AbsorbedDoseUnit[C])(
      implicit n: Numeric[A]) = 
      new AbsorbedDoseLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "AbsorbedDose"
    def primaryUnit = Grays
    def siUnit = Grays
    def units = Set(Grays, Rads, ErgsPerGram)
  }

  object Grays extends AbsorbedDoseUnitT with PrimaryUnit[C#T, C]
      with SiUnit {
    val symbol = "Gy"
  }

  object Rads extends AbsorbedDoseUnitT with UnitConverter[C#T, C] {
    val symbol = "rad"
    val conversionFactor = 0.01
  }

  object ErgsPerGram extends AbsorbedDoseUnitT with UnitConverter[C#T, C] {
    val symbol = "erg/g"
    val conversionFactor = 0.0001
  }

  object AbsorbedDoseConversions {
    lazy val gray = Grays(1)
    lazy val rad = Rads(1)
    lazy val ergsPerGram = ErgsPerGram(1)

    implicit class AbsorbedDoseConversions[A](a: A)(implicit num: Numeric[A]) {
      def grays = Grays(a)
      def rads = Rads(a)
      def ergsPerGram = ErgsPerGram(a)
    }
  }
}

