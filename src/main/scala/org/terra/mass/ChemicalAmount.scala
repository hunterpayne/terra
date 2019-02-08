/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package mass

import scala.reflect.ClassTag

import space.VolumeLike

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value in [[org.terra.mass.Moles]]
 */
final class ChemicalAmountLike[C <: TypeContext](
  val value: C#T, val unit: ChemicalAmountUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[ChemicalAmountLike[C], C#T, C] {

  import ops.chemicalAmountOps._

  type Volume = VolumeLike[C]

  def dimension: Dimension[ChemicalAmountLike[C], C#T, C] = ChemicalAmount

  def /(that: Volume) = ??? // returns SubstanceConcentration

  def toMoles = to(Moles)
  def toPoundMoles = to(PoundMoles)
}

trait ChemicalAmountUnit[C <: TypeContext] 
    extends UnitOfMeasure[ChemicalAmountLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) = 
    new ChemicalAmountLike[C](t, this)
}

trait ChemicalAmountOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]
  def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait ChemicalAmountUnitT extends ChemicalAmountUnit[C]

  object ChemicalAmount extends Dimension[ChemicalAmountLike[C], C#T, C] 
      with BaseDimension {
    private[mass] def apply[A](a: A, unit: ChemicalAmountUnit[C])(
      implicit n: Numeric[A]) = 
      new ChemicalAmountLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    val name = "ChemicalAmount"
    def primaryUnit = Moles
    def siUnit = Moles
    def units = Set(Moles, PoundMoles)
    def dimensionSymbol = "N"
  }

  object Moles extends ChemicalAmountUnitT with PrimaryUnit[C#T, C] 
      with SiBaseUnit {
    val symbol = "mol"
  }

  object PoundMoles extends ChemicalAmountUnitT {
    val symbol = "lb-mol"
    val conversionFactor = 453.59237
  }

  object ChemicalAmountConversions {
    lazy val mole = Moles(1)
    lazy val poundMole = PoundMoles(1)

    implicit class ChemicalAmountConversions[A](a: A)(implicit n: Numeric[A]) {
      def moles = Moles(a)
      def poundMoles = PoundMoles(a)
    }
  }
}

