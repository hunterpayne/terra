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

import org.terra.time.{ SecondTimeIntegral, TimeIntegral, TimeSquaredLike, TimeLike }
import org.terra.mass.MassLike

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value Double
 */
final class MomentumLike[C <: TypeContext](val value: C#T, val unit: MomentumUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[MomentumLike[C], C#T, C]
    with TimeIntegral[ForceLike[C], C#T, C#T, C]
    with SecondTimeIntegral[YankLike[C], C] {

  import ops.momentumOps._
  import ops.forceOps.Newtons
  import ops.timeOps.Seconds
  import ops.massOps.Kilograms
  import ops.velocityOps.MetersPerSecond

  type Velocity = VelocityLike[C]
  type Mass = MassLike[C]
  type TimeSquared = TimeSquaredLike[C]
  type Time = TimeLike[C]
  type Yank = YankLike[C]

  def dimension: Dimension[MomentumLike[C], C#T, C] = Momentum

  protected def timeDerived = Newtons(toNewtonSeconds)
  protected def time = Seconds(1)

  def /(that: Velocity)(implicit ops: TerraOps[C]): Mass = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    Kilograms(ops.div[C#T](this.toNewtonSeconds, that.toMetersPerSecond))
  }
  def /(that: Mass)(implicit ops: TerraOps[C]): Velocity = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    MetersPerSecond(ops.div[C#T](this.toNewtonSeconds, that.toKilograms))
  }

  def /(that: TimeSquared)(implicit ops: TerraOps[C]): Yank =
    this / that.time1 / that.time2
  def /(that: Yank)(implicit ops: TerraOps[C]): TimeSquared =
    (this / that.timeIntegrated) * time

  def toNewtonSeconds = to(NewtonSeconds)
}

trait MomentumUnit[C <: TypeContext] 
    extends UnitOfMeasure[MomentumLike[C], C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) = 
    new MomentumLike[C](t, this)
}

trait MomentumOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]
  def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait MomentumUnitT extends MomentumUnit[C]

  object Momentum extends Dimension[MomentumLike[C], C#T, C] {
    private[motion] def apply[A](a: A, unit: MomentumUnit[C])(
      implicit n: Numeric[A], ops: TerraOps[C]) =
      new MomentumLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(m: MassLike[C], v: VelocityLike[C]): MomentumLike[C] = {
      implicit val tag = ops.getClassTagT
      NewtonSeconds(ops.num.times(m.toKilograms, v.toMetersPerSecond))
    }
    def apply(value: Any) = parse(value)
    def name = "Momentum"
    def primaryUnit = NewtonSeconds
    def siUnit = NewtonSeconds
    def units = Set(NewtonSeconds)
  }

  object NewtonSeconds extends MomentumUnitT with PrimaryUnit[C#T, C]
      with SiUnit {
    val symbol = "Ns"
  }

  object MomentumConversions {
    lazy val newtonSecond = NewtonSeconds(1)

    implicit class MomentumConversions[A](a: A)(implicit n: Numeric[A]) {
      def newtonSeconds = NewtonSeconds(a)
    }
  }
}

