/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra

import scala.util.Success

/**
 * Base class for creating objects to manage quantities as Numeric.
 *
 * One limitation is the `times` operation which is not supported by every quantity type
 *
 * @tparam A Quantity type
 */
trait QuantityNumeric[A <: Quantity[A, T, C], T, C <: TypeContext] extends Numeric[A] {

  implicit val n: Numeric[T]
  implicit val ops: TerraOps[C]

  val unit: UnitOfMeasure[A, T, C] with PrimaryUnit[T, C]
  val dimension: Dimension[A, T, C]

  def plus(x: A, y: A) = x + y
  def minus(x: A, y: A) = x - y

  /**
   * `times` is not a supported Numeric operation for Quantities.
   * It is not possible to multiply a dimensional quantity by a like quantity and get another like quantity.
   * Applying this class in a way that uses this method will result in an UnsupportedOperationException being thrown.
   *
   * @param x Quantity[T]
   * @param y Quantity[T]
   * @return
   * @throws scala.UnsupportedOperationException for most types
   */
  def times(x: A, y: A): A = throw new UnsupportedOperationException("Numeric.times not supported for Quantities")
  def negate(x: A) = {
    implicit val tag = x.getTag
    unit(n.negate(x.to(unit)))
  }
  def fromInt(x: Int) = {
    implicit val n: Numeric[Int] = Numeric.IntIsIntegral
    unit[Int](n.fromInt(x))
  }
  def toInt(x: A) = n.toInt(x.to(unit))
  def toLong(x: A) = n.toLong(x.to(unit))
  def toFloat(x: A) = n.toFloat(x.to(unit))
  def toDouble(x: A) = n.toDouble(x.to(unit))
  def parseString(str: String): Option[A] = dimension.parse(str) match {
    case Success(a) => Some(a)
    case _ => None
  }

  def compare(x: A, y: A) = 
    if (toDouble(x) > toDouble(y)) 1 
    else if (toDouble(x) < toDouble(y)) -1 
    else 0
}

abstract class AbstractQuantityNumeric[A <: Quantity[A, C#T, C], C <: TypeContext](
  val dimension: Dimension[A, C#T, C])(
  //val unit: UnitOfMeasure[A, C#T, C] with PrimaryUnit[C#T, C])(
  implicit opsArg: TerraOps[C]) extends QuantityNumeric[A, C#T, C] {

  val unit = dimension.primaryUnit
  implicit val ops: TerraOps[C] = opsArg
  implicit val n: Numeric[C#T] = ops.num
}

abstract class AbstractQuantityNumericL[A <: Quantity[A, C#TL, C], C <: TypeContext](
  val dimension: Dimension[A, C#TL, C])(
  //val unit: UnitOfMeasure[A, C#TL, C] with PrimaryUnit[C#TL, C])(
  implicit opsArg: TerraOps[C]) extends QuantityNumeric[A, C#TL, C] {

  val unit = dimension.primaryUnit
  implicit val ops: TerraOps[C] = opsArg
  implicit val n: Numeric[C#TL] = ops.numL
}

abstract class AbstractQuantityNumericT[A <: Quantity[A, C#TT, C], C <: TypeContext](
  val dimension: Dimension[A, C#TT, C])(
  //val unit: UnitOfMeasure[A, C#TT, C] with PrimaryUnit[C#TT, C])(
  implicit opsArg: TerraOps[C]) extends QuantityNumeric[A, C#TT, C] {

  val unit = dimension.primaryUnit
  implicit val ops: TerraOps[C] = opsArg
  implicit val tag: PseudoClassTag[C#TT] = ops.getClassTagTT
  implicit val n: Numeric[C#TT] = ops.numT
}
