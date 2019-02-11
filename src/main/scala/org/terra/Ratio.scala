/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra

/**
 * Defines an interface and partial implementation for types that represent a ratio between any two quantities
 *
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @tparam A Quantity A
 * @tparam B Quantity B
 */
trait Ratio[A <: Quantity[A, T, C], B <: Quantity[B, T, C], T, C <: TypeContext] {
  def base: A
  def counter: B
  def convertToBase(q: B)(implicit ops: TerraOps[C]): A = {
    implicit val e: HasEnsureType[T] = base.makeEnsureType
    base * ops.ensureType[T](q / counter)
  }
  def convertToCounter(q: A)(implicit ops: TerraOps[C]): B = {
    implicit val e: HasEnsureType[T] = base.makeEnsureType
    counter * ops.ensureType[T](q / base)
  }
}

trait LikeRatio[A <: Quantity[A, T, C], T, C <: TypeContext] 
    extends Ratio[A, A, T, C] {
  def ratio(implicit ops: TerraOps[C]) = base / counter
  def inverseRatio(implicit ops: TerraOps[C]) = counter / base
}
