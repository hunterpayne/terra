
package org.terra
package mass

/**
 */
final class MolarMassLike[C <: TypeContext](
  val value: C#T, val unit: MolarMassUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[MolarMassLike[C], C#T, C] {

  import ops.molarMassOps._
  import ops.massOps.Kilograms
  import ops.chemicalAmountOps.Moles
  import ops.molarityOps.MolesPerKilogram

  type Mass = MassLike[C]
  type ChemicalAmount = ChemicalAmountLike[C]
  type Molarity = MolarityLike[C]

  def dimension: Dimension[MolarMassLike[C], C#T, C] = MolarMass

  def *(that: ChemicalAmount)(implicit ops: TerraOps[C]): Mass =
    Kilograms(ops.num.times(this.toKilogramsPerMole, that.toMoles))

  // inverse of Molarity
  def inv(implicit ops: TerraOps[C]): Molarity = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    MolesPerKilogram(ops.div[C#T](ops.convDouble(1.0), toKilogramsPerMole))
  }

  def toKilogramsPerMole = to(KilogramsPerMole)
  def toGramsPerMole = to(GramsPerMole)
}

trait MolarMassUnit[C <: TypeContext] 
    extends UnitOfMeasure[MolarMassLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) = new MolarMassLike[C](t, this)
}

trait MolarMassOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]
  //def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait MolarMassUnitT extends MolarMassUnit[C]

  object MolarMass extends Dimension[MolarMassLike[C], C#T, C] 
      with BaseDimension {
    private[mass] def apply[A](a: A, unit: MolarMassUnit[C])(
      implicit n: Numeric[A]) = 
      new MolarMassLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    val name = "MolarMass"
    def primaryUnit = KilogramsPerMole
    def siUnit = KilogramsPerMole
    def units = Set(KilogramsPerMole, GramsPerMole)
    def dimensionSymbol = "M"
  }

  import ops.massOps.{ Kilograms, Grams }
  import ops.chemicalAmountOps.Moles

  object KilogramsPerMole extends MolarMassUnitT with PrimaryUnit[C#T, C]
      with SiBaseUnit {
    val symbol = Kilograms.symbol + "/" + Moles.symbol
  }

  object GramsPerMole extends MolarMassUnitT {
    val symbol = Grams.symbol + "/" + Moles.symbol
    val conversionFactor = 1.0 / MetricSystem.Kilo
  }

  object MolarMassConversions {
    lazy val kilogramPerMole = KilogramsPerMole(1)
    lazy val gramPerMole = GramsPerMole(1)

    implicit class MolarMassConversions[A](a: A)(implicit n: Numeric[A]) {
      def kilogramsPerMole = KilogramsPerMole(a)
      def gramsPerMole = GramsPerMole(a)
    }
  }
}

