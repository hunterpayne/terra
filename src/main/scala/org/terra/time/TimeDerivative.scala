/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package time

trait HasTimeType[C <: TypeContext] {
  type Time = TimeLike[C]
}

/**
  * Represents a rate of change over time of the integral quantity
  *
  * @author  garyKeorkunian
  * @since   0.1
  *
  * @tparam A The type of quantity changing
  */
trait TimeDerivative[A <: Quantity[A, TI, C] with TimeIntegral[_, TI, TD, C], TD, TI, C <: TypeContext] extends HasTimeType[C] {

  protected[terra] def timeIntegrated: A
  protected[terra] def time: Time
  val terraOps: TerraOps[C]
  
  implicit val tttiConverter: HasConverter[C#TT, TI] = 
    terraOps.converters.tttConverter2.asInstanceOf[HasConverter[C#TT, TI]]

  /**
    * Returns the amount of change in the integral that will happen over the given Time
    *
    * @param that Time
    * @return
    */
  def *(that: Time)(implicit ops: TerraOps[C]): A = {
    import ops.converters._
    val perTime: Time = that / this.time.toSeconds
    timeIntegrated * ops.gconvTotal[C#TT, TI](perTime.toSeconds.asInstanceOf[C#TT])
  }
}

trait SecondTimeDerivative[A <: SecondTimeIntegral[_, C], C <: TypeContext] 
  extends HasTimeType[C] { self: TimeDerivative[_, C#T, C#T, C] ⇒

  type TimeSquared = TimeSquaredLike[C]
  protected[terra] def time: Time
  def *(that: TimeSquared)(implicit ops: TerraOps[C]): A
}

/**
  * Represents a Quantity type used as the integral of a time derivative
  *
  * @author  garyKeorkunian
  * @since   0.1
  *
  * @tparam A The Quantity type for the TimeDerivative for which this is the base
  */
trait TimeIntegral[A <: Quantity[A, TD, C] with TimeDerivative[_, TD, TI, C], TI, TD, C <: TypeContext] 
  extends HasTimeType[C] {

  protected def time: Time
  val terraOps: TerraOps[C]
  protected def timeDerived: A

  implicit val tttdConverter: HasConverter[C#TT, TD] = 
    terraOps.converters.tttConverter2.asInstanceOf[HasConverter[C#TT, TD]]
  implicit val tdttConverter: HasConverter[TD, C#TT] =
    terraOps.converters.tttConverter.asInstanceOf[HasConverter[TD, C#TT]]
  implicit val ttdConverter: HasConverter[C#T, TD] =
    new HasConverter[C#T, TD] {
      // use a casted identity function
      def conv(in: C#T): TD = in.asInstanceOf[TD]
    }

  /**
    * Returns the Time Derivative which represents a change of the underlying quantity equal to this
    * quantity over the given time.
    *
    * @param that Time
    * @return
    */
  def /(that: Time)(implicit ops: TerraOps[C]): A = {

    // this very opinioniated line of code says that all derivative types
    // are always C#T's and never any other kind of type
    implicit val ensureTD: HasEnsureType[TD] =
      terraOps.converters.ensureT.asInstanceOf[HasEnsureType[TD]]
    val amtTime: C#TT = this.time / that
    timeDerived * ops.ensureType[TD](ops.gconvTT[TD](amtTime))
  }
  def per(that: Time)(
    implicit ops: TerraOps[C], e: HasConverter[C#TT, TD]): A = /(that)

  /**
    * Returns the amount time required to achieve the given change in the Derivative
    *
    * @param that Derivative
    * @return
    */
  def /(that: A)(implicit ops: TerraOps[C]): Time = {
    val ratio: TD = timeDerived / that
    val tt: C#TT = ops.gconvTotal[TD, C#TT](ratio)
    that.time * tt
  }

  /**
    * Returns the Time Derivative of this Quantity based on the Frequency this Quantity occurs
    *
    * @param that Frequency - the rate at which this Quantity occurs
    * @return
    */
  def *(that: FrequencyLike[C])(implicit ops: TerraOps[C]): A = {
    val left: A = /(this.time)
    val dim: DimensionlessLike[C] = this.time * that
    val right: TD = ops.gconvT[TD](dim.toEach)
    left * right
  }
}

trait SecondTimeIntegral[A <: SecondTimeDerivative[_, C], C <: TypeContext] {
  self: TimeIntegral[_, C#T, C#T, C] ⇒

  type TimeSquared = TimeSquaredLike[C]

  def /(that: A)(implicit ops: TerraOps[C]): TimeSquared
  def /(that: TimeSquared)(implicit ops: TerraOps[C]): A
  def per(that: TimeSquared)(implicit ops: TerraOps[C]): A = /(that)
}
