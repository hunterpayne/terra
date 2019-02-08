/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra

import scala.reflect.ClassTag

/**
 * A Unit of Measure is used to define the scale of a quantity measurement
 *
 * Each Quantity Dimension must include at least one Unit of Measure, and one and only one Primary.
 * Other units of measure are defined with conversionFactors relative to the Primary.
 *
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @tparam A The type of Quantity being measured
 */
trait UnitOfMeasure[A <: Quantity[A, T, C], T, C <: TypeContext]
    extends Serializable {

  type TL = C#TL
  type TT = C#TT

  def getTag(implicit ops: TerraOps[C]): ClassTag[T] = 
    ops.getClassTagT.asInstanceOf[ClassTag[T]]

  /**
   * Factory method for creating instances of a Quantity in this UnitOfMeasure
   * @param n N - the Quantity's value in terms of this UnitOfMeasure
   * @return
    */
  def apply(t: T)(implicit tag: ClassTag[T], ops: TerraOps[C]): A
  def apply[N](a: N)(implicit n: Numeric[N], ops: TerraOps[C]): A = {
    implicit val tag: ClassTag[T] = getTag
    val t: C#T = ops.convDouble(n.toDouble(a))
    apply(t.asInstanceOf[T]) // this method must be overridden when C#T != T
  }

  private[terra] def makeEnsureType(
    implicit ops: TerraOps[C]): HasEnsureType[T] =
    ops.converters.ensureT.asInstanceOf[HasEnsureType[T]]

  private[terra] def makeDoubleConverter(
    implicit ops: TerraOps[C]): HasConverter[Double, T] = 
    ops.converters.dtConverter.asInstanceOf[HasConverter[Double, T]]
  private[terra] def makeLongConverter(
    implicit ops: TerraOps[C]): HasConverter[Long, T] = 
    ops.converters.tltConverter.asInstanceOf[HasConverter[Long, T]]
  //implicit val tdttConverter: HasConverter[Double, C#T] =
    //ops.converters.tttConverter

  /**
   * Extractor method for getting the Numeric value of a Quantity in this UnitOfMeasure
   * @param q A - The Quantity being matched
   * @return
   */
  def unapply(q: A) = Some(q.to(this))

  /**
   * Symbol used when representing Quantities in this UnitOfMeasure
   * @return
   */
  def symbol: String

  /**
   * Defines a signature for converting a quantity from this UOM to the Value UOM
   * @return
   */
  protected def converterFrom(implicit ops: TerraOps[C]): T ⇒ T

  /**
   * Defines a signature for converting a quantity to this UOM from the Value UOM
   * @return
   */
  protected def converterTo(implicit ops: TerraOps[C]): T ⇒ T

  /**
   * Applies the converterTo method to a value
   * @param n N value in terms of the ValueUnit
   * @param num Numeric[N]
   * @tparam N Type
   * @return
   */
  final def convertTo(n: T)(implicit ops: TerraOps[C]): T = converterTo.apply(n)

  /**
   * Applies the converterFrom method to a value
   *
   * @param n N value in terms of this Unit
   * @param num Numeric[N]
   * @tparam N Type
   * @return
   */
  final def convertFrom(n: T)(implicit ops: TerraOps[C]): T = converterFrom.apply(n)
}

/**
 * A Unit of Measure that require a simple multiplier for converting to and from the underlying value's unit
 */
trait UnitConverter[T, C <: TypeContext] { uom: UnitOfMeasure[_, T, C] ⇒

  /**
   * Defines a multiplier value relative to the Quantity's [[org.terra.PrimaryUnit]]
   *
   * @return
   */
  // TODO how to handle this with types longer than double??? BigDecimal???
  protected def convFactor(implicit ops: TerraOps[C]): C#T = {
    //implicit val convT: HasConverter[Double, T] = makeDoubleConverter
    //ops.gconvTotal[Double, T](conversionFactor)
    ops.convDouble(conversionFactor)
  }
  def conversionFactor: Double

  /**
   * Implements the converterTo method as a simple quotient of the value and the multiplier
   * @return
   */
  protected def converterTo(implicit ops: TerraOps[C]): T ⇒ T = {
    implicit val ensureT: HasEnsureType[T] = makeEnsureType
    implicit val otherEnsureT = ops.converters.ensureT
    implicit val cTag: ClassTag[C#T] = ops.getClassTagT
    implicit val tag: ClassTag[T] = getTag
    value ⇒ {
      ops.ensureType[T](ops.div[C#T](
        ops.ensureType[C#T](value), 
        convFactor))
    }
  }

  /**
   * Implements the converterFrom method as a simple product of the value and the multiplier
   * @return
   */
  protected def converterFrom(implicit ops: TerraOps[C]): T ⇒ T = {
    implicit val ensureT: HasEnsureType[T] = makeEnsureType
    implicit val otherEnsureT = ops.converters.ensureT
    implicit val tag: ClassTag[T] = getTag
    value ⇒ {
      ops.ensureType[T](ops.num.times(
        ops.ensureType[C#T](value), 
        convFactor))
    }
  }
}

/**
 * Identifies the Unit of Measure with a conversionFactor of 1.0.
 *
 * It is used as the intermediary unit during conversions
 *
 * Each Quantity should have one and only one ValueUnit
 */
trait PrimaryUnit[T, C <: TypeContext] extends UnitConverter[T, C] { 
  uom: UnitOfMeasure[_, T, C] ⇒

  /**
   * Implements the converterTo method to just return the underlying value
   * @return
   */
  override final def converterTo(implicit ops: TerraOps[C]): T ⇒ T = value ⇒ value

  /**
   * Implements the converterFrom method to just return the underlying value
   * @return
   */
  override final def converterFrom(implicit ops: TerraOps[C]): T ⇒ T = value ⇒ value

  /**
   * Value unit multiplier is always equal to 1
   */
  final def conversionFactor: Double = 1.0
}

/**
 * A marker trait identifying SI Units
 */
trait SiUnit

/**
 * A marker trait identifying SI Base Units
 */
trait SiBaseUnit extends SiUnit
