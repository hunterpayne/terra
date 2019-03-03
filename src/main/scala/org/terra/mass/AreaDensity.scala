/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package mass

import space.AreaLike

/**
 * @author  garyKeorkunian
 * @since   0.2.3
 *
 * @param value Double
 */
final class AreaDensityLike[C <: TypeContext](
  val value: C#T, val unit: AreaDensityUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[AreaDensityLike[C], C#T, C] {

  import ops.areaDensityOps._
  import ops.massOps.Kilograms

  type Area = AreaLike[C]
  type Mass = MassLike[C]

  def dimension: Dimension[AreaDensityLike[C], C#T, C] = AreaDensity

  def *(that: Area)(implicit ops: TerraOps[C]): Mass = 
    Kilograms(ops.num.times(this.toKilogramsPerSquareMeter, that.toSquareMeters))

  def toKilogramsPerSquareMeter = to(KilogramsPerSquareMeter)
  def toGramsPerSquareCentimeter = to(GramsPerSquareCentimeter)
  def toKilogramsPerHectare = to(KilogramsPerHectare)
  def toPoundsPerAcre = to(PoundsPerAcre)
}

trait AreaDensityUnit[C <: TypeContext] 
    extends UnitOfMeasure[AreaDensityLike[C], C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) = new AreaDensityLike[C](t, this)
}

trait AreaDensityOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]
  //def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait AreaDensityUnitT extends AreaDensityUnit[C]

  /**
    * Factory singleton for [[org.terra.mass.AreaDensity]] values
    */
  object AreaDensity extends Dimension[AreaDensityLike[C], C#T, C] {
    private[mass] def apply[A](a: A, unit: AreaDensityUnit[C])(
      implicit n: Numeric[A]) = 
      new AreaDensityLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(mass: MassLike[C], area: AreaLike[C]): AreaDensityLike[C] = {
      implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
      implicit val tag: PseudoClassTag[C#T] = ops.getClassTagT
      KilogramsPerSquareMeter(ops.div[C#T](mass.toKilograms, area.toSquareMeters))
    }
    def apply(value: Any) = parse(value)
    def name = "AreaDensity"
    def primaryUnit = KilogramsPerSquareMeter
    def siUnit = KilogramsPerSquareMeter
    def units = Set(KilogramsPerSquareMeter, KilogramsPerHectare, GramsPerSquareCentimeter, PoundsPerAcre)
  }

  object KilogramsPerSquareMeter extends AreaDensityUnitT 
      with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = "kg/m²"
  }

  object KilogramsPerHectare extends AreaDensityUnitT with UnitConverter[C#T, C] {
    val symbol = "kg/hectare"
    val conversionFactor = 1/(100*100d)
  }

  object GramsPerSquareCentimeter extends AreaDensityUnitT
      with UnitConverter[C#T, C] with SiUnit {
    val symbol = "g/cm²"
    val conversionFactor = (100*100d)/1000d
  }

  import ops.massOps.Pounds
  import ops.areaOps.Acres

  object PoundsPerAcre extends AreaDensityUnitT with UnitConverter[C#T, C] {
    val symbol = s"${Pounds.symbol}/${Acres.symbol}"
    // Base unit is kg/m^2
    import ops.massOps.MassConversions.{ pound, kilogram }
    import ops.areaOps.AreaConversions.{ acre, squareMeter }
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    implicit val tag: PseudoClassTag[C#T] = ops.getClassTagT
    val conversionFactor =
      ops.num.toDouble(ops.div[C#T]((pound / kilogram), (acre / squareMeter)))
  }

  object AreaDensityConversions {
    lazy val kilogramPerSquareMeter = KilogramsPerSquareMeter(1)
    lazy val kilogramPerHectare = KilogramsPerHectare(1)
    lazy val gramPerSquareCentimeter = GramsPerSquareCentimeter(1)
    lazy val poundsPerAcre = PoundsPerAcre(1)

    implicit class AreaDensityConversions[A](a: A)(implicit n: Numeric[A]) {
      def kilogramsPerSquareMeter = KilogramsPerSquareMeter(a)
      def kilogramsPerHectare = KilogramsPerHectare(a)
      def gramsPerSquareCentimeter = GramsPerSquareCentimeter(a)
    }
  }
}

