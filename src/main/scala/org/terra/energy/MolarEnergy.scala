
package org.terra
package energy

import mass.ChemicalAmountLike

/**
  *
  * @author Nicolas Vinuesa
  * @since 1.4
  *
  * @param value Double
  */
final class MolarEnergyLike[C <: TypeContext](
  val value: C#T, val unit: MolarEnergyUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[MolarEnergyLike[C], C#T, C] {

  import ops.molarEnergyOps._
  import ops.energyOps.Joules

  type Energy = EnergyLike[C]
  type ChemicalAmount = ChemicalAmountLike[C]

  def dimension: Dimension[MolarEnergyLike[C], C#T, C] = MolarEnergy

  def *(that: ChemicalAmount)(implicit ops: TerraOps[C]): Energy = 
    Joules(ops.num.times(this.toJoulesPerMole, that.toMoles))

  def toJoulesPerMole = to(JoulesPerMole)
}

trait MolarEnergyUnit[C <: TypeContext] 
    extends UnitOfMeasure[MolarEnergyLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) = 
    new MolarEnergyLike[C](t, this)
}

trait MolarEnergyOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]

  trait MolarEnergyUnitT extends MolarEnergyUnit[C]

  object MolarEnergy extends Dimension[MolarEnergyLike[C], C#T, C] {
    private[energy] def apply[A](a: A, unit: MolarEnergyUnit[C])(
      implicit n: Numeric[A]) = 
      new MolarEnergyLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "MolarEnergy"
    def primaryUnit = JoulesPerMole
    def siUnit = JoulesPerMole
    def units = Set(JoulesPerMole)
  }

  object JoulesPerMole extends MolarEnergyUnitT with PrimaryUnit[C#T, C]
      with SiUnit {
    val symbol = ops.energyOps.Joules.symbol + "/" + 
      ops.chemicalAmountOps.Moles.symbol
  }

  object MolarEnergyConversions {
    lazy val joulePerMole = JoulesPerMole(1)

    implicit class MolarEnergyConversions[A](a: A)(implicit num: Numeric[A]) {
      def joulesPerMole = JoulesPerMole(a)
    }
  }
}

