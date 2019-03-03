/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra

import java.util.Objects

import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal.RoundingMode.RoundingMode

/**
 * A base class for measurable quantities, instances of which contain a value and a unit
 *
 * @author  garyKeorkunian
 * @since   0.1
 *
 */
abstract class Quantity[A <: Quantity[A, T, C], T, C <: TypeContext](
  implicit ops: TerraOps[C])
    extends Serializable with Ordered[A] { self: A =>

  val terraOps: TerraOps[C] = ops

  //def getTag: ClassTag[T] = ops.getClassTagT.asInstanceOf[ClassTag[T]]
  def getTag: PseudoClassTag[T] = 
    ops.getClassTagT.asInstanceOf[PseudoClassTag[T]]
  def getNumeric: Numeric[T] = ops.num.asInstanceOf[Numeric[T]]
  //implicit val tag: ClassTag[T] = getTag
  implicit val tag: PseudoClassTag[T] = getTag
  implicit val num: Numeric[T] = getNumeric

  def makeEnsureType(implicit ops: TerraOps[C]): HasEnsureType[T] =
    ops.converters.ensureT.asInstanceOf[HasEnsureType[T]]

  def makeFromCurrencyConverter(
    implicit ops: TerraOps[C]): HasConverter[C#TC, T] =
    ops.converters.tctConverter.asInstanceOf[HasConverter[C#TC, T]]

  def makeToCurrencyConverter(
    implicit ops: TerraOps[C]): HasConverter[T, C#TC] =
    ops.converters.ttcConverter.asInstanceOf[HasConverter[T, C#TC]]

  /**
   * The value of the quantity given the unit
   * @return Double
   */
  def value: T

  /**
   * The Unit of Measure the value represents
   * @return UnitOfMeasure[A, T]
   */
  def unit: UnitOfMeasure[A, T, C]

  /**
   * The Dimension this quantity represents
   * @return
   */
  def dimension: Dimension[A, T, C]

  /**
   * Add two like quantities
   * @param that Quantity
   * @return Quantity
   */
  def plus(that: A)(implicit ops: TerraOps[C]): A = 
    unit(num.plus(this.value, that.to(unit)))
  def +(that: A)(implicit ops: TerraOps[C]): A = plus(that)

  /**
   * Subtract two like quantities
   * @param that Quantity
   * @return Quantity
   */
  def minus(that: A)(implicit ops: TerraOps[C]): A = plus(that.negate)
  def -(that: A)(implicit ops: TerraOps[C]): A = minus(that)

  /**
   * Multiply this quantity by some number
   * @param that Double
   * @return Quantity
   */
  def times(that: T)(implicit ops: TerraOps[C]): A =
    unit(num.times(this.value, that))
  def *(that: T)(implicit ops: TerraOps[C]): A = times(that)
  //def *(that: Double)(implicit ops: TerraOps[C]): A = {
    //implicit val ensure: HasEnsureType[T] = makeEnsureType
    //times(ops.ensureType[T](that))
  //}
  //def *(that: BigDecimal)(implicit ops: TerraOps[C]): A = {
    //implicit val ensure: HasEnsureType[T] = makeEnsureType
    //times(ops.ensureType[T](that))
  //}

  /**
   * Divide this quantity by some number
   * @param that Double
   * @return Quantity
   */
  def divide(that: T)(implicit ops: TerraOps[C]): A = {
    implicit val ensure: HasEnsureType[T] = makeEnsureType
    unit(ops.div[T](this.value, that))
  }
  def /(that: T)(implicit ops: TerraOps[C]): A = divide(that)

  /**
   * Divide this quantity by a like quantity
   * @param that Quantity
   * @return Double
   */
  def divide(that: A)(implicit ops: TerraOps[C]): C#T = {
    implicit val ensure: HasEnsureType[C#T] = ops.converters.ensureT
    implicit val tag: PseudoClassTag[C#T] = ops.getClassTagT
    ops.div[C#T](
      ops.ensureType[C#T](this.value), ops.ensureType[C#T](that.to(unit)))
  }
  def /(that: A)(implicit ops: TerraOps[C]): C#T = divide(that)

  /**
   * Returns the remainder of a division by a number
   * @param that Quantity
   * @return Quantity
   */
  def remainder(that: T)(implicit ops: TerraOps[C]): A = {
    implicit val ensure: HasEnsureType[T] = makeEnsureType
    unit(ops.mod(value, that))
  }
  def %(that: T)(implicit ops: TerraOps[C]): A = remainder(that)

  /**
   * Returns the remainder of a division by a like quantity
   * @param that Quantity
   * @return Double
   */
  def remainder(that: A)(implicit ops: TerraOps[C]): T = {
    implicit val ensure: HasEnsureType[T] = makeEnsureType
    ops.mod[T](value, that.to(unit))
  }
  def %(that: A)(implicit ops: TerraOps[C]): T = remainder(that)

  /**
   * Returns a Pair that includes the result of divideToInteger and remainder
   * @param that Double
   * @return (Quantity, Quantity)
   */
  def divideAndRemainder(that: T)(implicit ops: TerraOps[C]): (A, A) = 
    (divide(that).floor, remainder(that))
  def /%(that: T)(implicit ops: TerraOps[C]) = divideAndRemainder(that)

  /**
   * Returns a Pair that includes the result of divideToInteger and remainder
   * @param that Quantity
   * @return (Double, Quantity)
   */
  def divideAndRemainder(that: A)(implicit ops: TerraOps[C]): (T, A) = {
    implicit val ensure: HasEnsureType[T] = makeEnsureType
    (ops.floorT[T](ops.ensureType[T](divide(that))), remainder(that.to(unit)))
  }
  def /%(that: A)(implicit ops: TerraOps[C]) = divideAndRemainder(that)

  /**
   * Returns the negative value of this Quantity
   * @return Quantity
   */
  def negate(implicit ops: TerraOps[C]): A = unit(num.negate(value))
  def unary_-()(implicit ops: TerraOps[C]): A = negate

  /**
   * Returns the absolute value of this Quantity
   * @return Quantity
   */
  def abs: A = unit(num.abs(value))

  /**
   * Returns the smallest (closest to negative infinity) Quantity value that is greater than or equal to the argument and is equal to a mathematical integer.
   *
   * @see java.lang.Math#ceil(double)
   * @return Quantity
   */
  def ceil: A = {
    implicit val ensure: HasEnsureType[T] = makeEnsureType
    unit(ops.ceilT[T](value))
  }

  /**
   * Returns the largest (closest to positive infinity) Quantity value that is less than or equal to the argument and is equal to a mathematical integer
   *
   * @see java.lang.Math#floor(double)
   * @return Quantity
   */
  def floor: A = {
    implicit val ensure: HasEnsureType[T] = makeEnsureType
    unit(ops.floorT[T](value))
  }
  /**
   * Returns the Quantity value that is closest in value to the argument and is equal to a mathematical integer.
   *
   * @see java.lang.Math#rint(double)
   * @return Quantity
   */
  def rint: A = {
    implicit val ensure: HasEnsureType[T] = makeEnsureType
    unit(ops.rintT[T](value))
  }

  /**
    * Returns the Quantity with its coefficient value rounded using scale and mode.  The unit is maintained.
    *
    * @param scale Int - scale of the value to be returned
    * @param mode RoundingMode - defaults to HALF_EVEN
    * @return Quantity
    */
  def rounded(scale: Int, mode: RoundingMode = RoundingMode.HALF_EVEN): A = {
    implicit val ensure: HasEnsureType[T] = makeEnsureType
    unit(ops.roundT[T](value, scale, mode))
  }

  /**
   * Override of equals method
   *
   * @param that must be of matching value and unit
   * @return
   */
  override def equals(that: Any) = that match {
    case x: Quantity[A, T, C] if x.dimension == dimension ⇒ value == x.to(unit)
    case _ ⇒ false
  }

  /**
   * Override of hashCode
   *
   * @return
   */
  override def hashCode() = {
    Objects.hash(dimension, Double.box(num.toDouble(to(dimension.primaryUnit))))
  }

  /**
   * Returns boolean result of approximate equality comparison
   * @param that Quantity
   * @param tolerance Quantity
   * @return
   */
  def approx(that: A)(implicit tolerance: A) = 
    that within this.plusOrMinus(tolerance)
  /** approx */
  def =~(that: A)(implicit tolerance: A) = approx(that)
  /** approx */
  def ≈(that: A)(implicit tolerance: A) = approx(that)
  /** approx */
  def ~=(that: A)(implicit tolerance: A) = approx(that)

  /**
   * Implements Ordered.compare
   * @param that Quantity
   * @return Int
   */
  def compare(that: A) = if (num.gt(this.value, that.to(unit))) 1 else if (num.lt(this.value, that.to(unit))) -1 else 0

  /**
   * Returns the max of this and that Quantity
   * @param that Quantity
   * @return Quantity
   */
  def max(that: A): A =
    if (num.gteq(this.value, that.to(unit))) this else that

  /**
   * Returns the min of this and that Quantity
   * @param that Quantity
   * @return Quantity
   */
  def min(that: A): A =
    if (num.lteq(this.value, that.to(unit))) this else that

  /**
   * Returns a QuantityRange representing the range for this value +- that
   * @param that Quantity
   * @return QuantityRange
   */
  def plusOrMinus(that: A): QuantityRange[A, T, C] = QuantityRange(this - that, this + that)
  def +-(that: A) = plusOrMinus(that)

  /**
   * Returns a QuantityRange that goes from this to that
   * @param that Quantity
   * @return QuantityRange
   */
  def toThat(that: A): QuantityRange[A, T, C] = QuantityRange(this / num.fromInt(1), that)

  /**
   * Returns true if this value is within (contains) the range
   * @param range QuantityRange
   * @return Boolean
   */
  def within(range: QuantityRange[A, T, C]) = range.contains(self)

  /**
   * Returns true if this value is not within (contains) the range
   * @param range QuantityRange
   * @return Boolean
   */
  def notWithin(range: QuantityRange[A, T, C]) = !range.contains(self)

  /**
   * Returns a Double representing the quantity in terms of the supplied unit
   * {{{
   *   val d = Feet(3)
   *   (d to Inches) should be(36)
   * }}}
   * @param uom UnitOfMeasure[A, T]
   * @return Double
   */
  def to(uom: UnitOfMeasure[A, T, C]): T = uom match {
    case u if u == this.unit ⇒ value
    case _                   ⇒ uom.convertTo(this.unit.convertFrom(value))
  }

  /**
   * Returns an equivalent Quantity boxed with the supplied Unit
   * @param uom UnitOfMeasure[A, T]
   * @return Quantity
   */
  def in(uom: UnitOfMeasure[A, T, C]): A = uom match {
    case u if u == this.unit ⇒ this
    case _                   ⇒ uom(uom.convertTo(this.unit.convertFrom(value)))
  }

  /**
   * Returns a string representing the quantity's value in unit
   * @return String
   */
  override def toString: String = toString(unit)

  /**
   * Returns a string representing the quantity's value in the given `unit`
   * @param uom UnitOfMeasure[A, T] with UnitConverter
   * @return String
   */
  def toString(uom: UnitOfMeasure[A, T, C]): String = {
    implicit val ensure: HasEnsureType[T] = makeEnsureType
    crossFormat[T](ops.ensureType[T](to(uom))) + " " + uom.symbol
    //s"${s} ${uom.symbol}"
  }

  /**
   * Returns a string representing the quantity's value in the given `unit` in the given `format`
   * @param uom UnitOfMeasure[A, T] with UnitConverter
   * @param format String containing the format for the value (ie "%.3f")
   * @return String
   */
  def toString(uom: UnitOfMeasure[A, T, C], format: String): String = "%s %s".format(format.format(to(uom)), uom.symbol)

  /**
   * Returns a tuple representing the numeric value and the unit's symbol
   * @return
   */
  def toTuple: (T, String) = (value, unit.symbol)

  /**
   * Returns a pair representing the numeric value and the uom's symbol
   * @param uom UnitOfMeasure[A, T]
   * @return
   */
  def toTuple(uom: UnitOfMeasure[A, T, C]): (T, String) = (to(uom), uom.symbol)

  /**
   * Applies a function to the underlying value of the Quantity, returning a new Quantity in the same unit
   * @param f Double => Double function
   * @return
   */
  def map(f: T ⇒ T): A = unit(f(value))
}

