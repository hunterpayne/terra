
package org.terra
package mass

/**
  * This is molarity.  Molar concentration is concentration.
  */
final class MolarityLike[C <: TypeContext](
  val value: C#T, val unit: MolarityUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[MolarityLike[C], C#T, C] {

  import ops.molarityOps._
  import ops.massOps.Kilograms
  import ops.chemicalAmountOps.Moles
  import ops.molarMassOps.KilogramsPerMole

  type Mass = MassLike[C]
  type ChemicalAmount = ChemicalAmountLike[C]
  type MolarMass = MolarMassLike[C]

  def dimension: Dimension[MolarityLike[C], C#T, C] = Molarity

  def *(that: Mass)(implicit ops: TerraOps[C]): ChemicalAmount =
    Moles(ops.num.times(this.toMolesPerKilogram, that.toKilograms))

  // inverse of MolarMass
  def inv(implicit ops: TerraOps[C]): MolarMass = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    KilogramsPerMole(ops.div[C#T](ops.convDouble(1.0), toMolesPerKilogram))
  }

  def toMolesPerKilogram = to(MolesPerKilogram)
}

trait MolarityUnit[C <: TypeContext] 
    extends UnitOfMeasure[MolarityLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) = new MolarityLike[C](t, this)
}

trait MolarityOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]
  //def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait MolarityUnitT extends MolarityUnit[C]

  object Molarity extends Dimension[MolarityLike[C], C#T, C] 
      with BaseDimension {
    private[mass] def apply[A](a: A, unit: MolarityUnit[C])(
      implicit n: Numeric[A]) = 
      new MolarityLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    val name = "Molarity"
    def primaryUnit = MolesPerKilogram
    def siUnit = MolesPerKilogram
    def units = Set(MolesPerKilogram)
    def dimensionSymbol = "molar"
  }

  import ops.massOps.Kilograms
  import ops.chemicalAmountOps.Moles

  object MolesPerKilogram extends MolarityUnitT with PrimaryUnit[C#T, C]
      with SiBaseUnit {
    val symbol = Moles.symbol + "/" + Kilograms.symbol
  }

  object MolarityConversions {
    lazy val molePerKilogram = MolesPerKilogram(1)

    implicit class MolarityConversions[A](a: A)(implicit n: Numeric[A]) {
      def molesPerKilogram = MolesPerKilogram(a)
    }
  }
}

