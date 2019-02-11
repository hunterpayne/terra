/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package motion

import scala.reflect.ClassTag

import org.terra.time.{ SecondTimeDerivative, TimeDerivative, TimeSquaredLike }

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value Double
 */
final class YankLike[C <: TypeContext](val value: C#T, val unit: YankUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[YankLike[C], C#T, C]
    with TimeDerivative[ForceLike[C], C#T, C#T, C]
    with SecondTimeDerivative[MomentumLike[C], C] {

  import ops.yankOps._
  import ops.forceOps.Newtons
  import ops.timeOps.Seconds

  type Momentum = MomentumLike[C]

  def dimension: Dimension[YankLike[C], C#T, C] = Yank

  protected[terra] def timeIntegrated = Newtons(toNewtonsPerSecond)
  protected[terra] def time = Seconds(1)

  def *(that: TimeSquared)(implicit ops: TerraOps[C]): Momentum =
    this * that.time1 * that.time2

  def toNewtonsPerSecond = to(NewtonsPerSecond)
}

trait YankUnit[C <: TypeContext] extends UnitOfMeasure[YankLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) = 
    new YankLike[C](t, this)
}

trait YankOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]
  def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait YankUnitT extends YankUnit[C]

  object Yank extends Dimension[YankLike[C], C#T, C] {
    private[motion] def apply[A](a: A, unit: YankUnit[C])(
      implicit n: Numeric[A], ops: TerraOps[C]) =
      new YankLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "Yank"
    def primaryUnit = NewtonsPerSecond
    def siUnit = NewtonsPerSecond
    def units = Set(NewtonsPerSecond)
  }


  object NewtonsPerSecond extends YankUnitT with PrimaryUnit[C#T, C]
      with SiUnit {
    val symbol = "N/s"
  }

  object YankConversions {
    lazy val newtonPerSecond = NewtonsPerSecond(1)

    implicit class YankConversions[A](a: A)(implicit n: Numeric[A]) {
      def newtonsPerSecond = NewtonsPerSecond(a)
    }
  }
}

