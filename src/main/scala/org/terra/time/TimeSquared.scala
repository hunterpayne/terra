/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package time

/**
  * Represents an intermediate value used in 2nd derivative time calculations
  *
  * Create objects by calling the Time.squared method.
  *
  * q1 / TimeSquared(t1, t2) == q1 / t1 / t2 == q1 / (t1 * t2)
  *
  * q2 * TimeSquared(t1, t2) == q2 * t1 * t2
  *
  * q1 / t1.squared == q1 / t1 / t1
  *
  * q2 * t1.squared == q2 * t1 * t1
  *
  * where q1 is a second degree time integral
  * and q2 is a second degree time derivative
  *
  * @param time1 Time
  * @param time2 Time
  *
  * @author garyKeorkunian
  * @since 0.5.1
  */
case class TimeSquaredLike[C <: TypeContext](
  time1: TimeLike[C], time2: TimeLike[C])(
  implicit ops: TerraOps[C]) {

  def this(time: TimeLike[C])(implicit ops: TerraOps[C]) = this(time, time)

  def squareRoot = {
    implicit val e: HasEnsureType[C#TT] = ops.converters.ensureTT
    implicit val tag: PseudoClassTag[C#TT] = ops.getClassTagTT
    implicit val num: Numeric[C#TT] = ops.numT
    time1.unit(ops.ensureType[C#TT](
      ops.sqrtT[C#TT](ops.nt[C#TT].times(time1.value, time2.to(time1.unit)))))
  }

  def *(that: SecondTimeDerivative[_, C]) =
    that * (this.time1 * this.time2)
}

trait TimeSquaredUnit[C <: TypeContext] {
  protected def timeUnit: TimeUnit[C]
  def apply(value: C#TT)(implicit ops: TerraOps[C]): TimeSquaredLike[C] = {
    implicit val num: Numeric[C#TT] = ops.numT
    new TimeSquaredLike[C](timeUnit(value), timeUnit(value))
  }
}

trait TimeSquaredOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]

  object TimeSquared {
    def apply(time1: TimeLike[C], time2: TimeLike[C]): TimeSquaredLike[C] = 
      TimeSquaredLike[C](time1, time2)
    def apply(time: TimeLike[C]): TimeSquaredLike[C] = 
      TimeSquaredLike[C](time, time)
  }

  object SecondsSquared extends TimeSquaredUnit[C] {
    protected def timeUnit = ops.timeOps.Seconds
  }
  object MinutesSquared extends TimeSquaredUnit[C] {
    protected def timeUnit = ops.timeOps.Minutes
  }
  object HoursSquared extends TimeSquaredUnit[C] {
    protected def timeUnit = ops.timeOps.Hours
  }
}
