
package org.terra
package mass

import space.{ VolumeLike, MolarVolumeLike }

/**
  * This is molar concentration.  Molarity is in molarity.
  */
final class ConcentrationLike[C <: TypeContext](
  val value: C#T, val unit: ConcentrationUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[ConcentrationLike[C], C#T, C] {

  import ops.concentrationOps._
  import ops.chemicalAmountOps.Moles
  import ops.molarVolumeOps.CubicMetersPerMole

  type Volume = VolumeLike[C]
  type ChemicalAmount = ChemicalAmountLike[C]
  type MolarVolume = MolarVolumeLike[C]

  def dimension: Dimension[ConcentrationLike[C], C#T, C] = Concentration

  def *(that: Volume)(implicit ops: TerraOps[C]): ChemicalAmount =
    Moles(ops.num.times(toMolesPerCubicMeter, that.toCubicMeters))

  // inverse of MolarVolume
  def inv(implicit ops: TerraOps[C]): MolarVolume = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    CubicMetersPerMole(ops.div[C#T](ops.convDouble(1.0), toMolesPerCubicMeter))
  }

  def toMolesPerCubicMeter = to(MolesPerCubicMeter)
  def toMolesPerLitre = to(MolesPerLitre)
  def toMolesPerMillilitre = to(MolesPerMillilitre)
}

trait ConcentrationUnit[C <: TypeContext] 
    extends UnitOfMeasure[ConcentrationLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) = 
    new ConcentrationLike[C](t, this)
}

trait ConcentrationOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]
  //def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait ConcentrationUnitT extends ConcentrationUnit[C]

  object Concentration extends Dimension[ConcentrationLike[C], C#T, C] 
      with BaseDimension {
    private[mass] def apply[A](a: A, unit: ConcentrationUnit[C])(
      implicit n: Numeric[A]) = 
      new ConcentrationLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    val name = "Concentration"
    def primaryUnit = MolesPerCubicMeter
    def siUnit = MolesPerCubicMeter
    def units = Set(MolesPerCubicMeter, MolesPerLitre, MolesPerMillilitre)
    def dimensionSymbol = "cᵢ"
  }

  import ops.volumeOps.{ CubicMeters, Litres, Millilitres }
  import ops.chemicalAmountOps.Moles

  object MolesPerCubicMeter extends ConcentrationUnitT with PrimaryUnit[C#T, C] 
      with SiBaseUnit {
    val symbol = Moles.symbol + "/" + CubicMeters.symbol
  }

  object MolesPerLitre extends ConcentrationUnitT {
    val symbol = Moles.symbol + "/" + Litres.symbol
    val conversionFactor = Litres.conversionFactor
  }

  object MolesPerMillilitre extends ConcentrationUnitT {
    val symbol = Moles.symbol + "/" + Millilitres.symbol
    val conversionFactor = Millilitres.conversionFactor
  }

  object ConcentrationConversions {
    lazy val molePerCubicMeter = MolesPerCubicMeter(1)
    lazy val molePerLitre = MolesPerLitre(1)
    lazy val molePerMillilitre = MolesPerMillilitre(1)

    implicit class ConcentrationConversions[A](a: A)(implicit n: Numeric[A]) {
      def molesPerCubicMeter = MolesPerCubicMeter(a)
      def molesPerLitre = MolesPerLitre(a)
      def molesPerMillilitre = MolesPerMillilitre(a)
    }
  }
}

