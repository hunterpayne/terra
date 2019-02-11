/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra

import scala.reflect.ClassTag

import org.terra.time._
import org.terra.information._

/**
  * Represents a quantity of some thing for which there is no dimension.
  *
  * This may be used to represent counts or other discrete amounts of everyday life,
  * but may also represent ratios between like quantities where the units have cancelled out.
  *
  * @param value Double the amount
  */
final class DimensionlessLike[C <: TypeContext](
  val value: C#T, val unit: DimensionlessUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[DimensionlessLike[C], C#T, C]
    with TimeIntegral[FrequencyLike[C], C#T, C#T, C] {

  type T = C#T
  type Frequency = FrequencyLike[C]
  type Dimensionless = DimensionlessLike[C]

  import ops.dimensionlessOps._
  import ops.frequencyOps.Hertz
  import ops.timeOps.Seconds

  implicit val tagT = getTag

  protected def timeDerived = Hertz(toEach)
  protected[terra] def time = Seconds(1)

  val dimension: Dimension[Dimensionless, C#T, C] = Dimensionless

  def *(that: Dimensionless)(implicit ops: TerraOps[C]): Dimensionless = 
    this * that.toEach
  def *[B <: Quantity[B, T, C]](that: B)(implicit ops: TerraOps[C]): B =
    that * toEach
  def +(that: T)(implicit ops: TerraOps[C]): Dimensionless = {
    implicit val tagT = getTag
    plus(Each(that))
  }

  def toPercent: T = to(Percent)
  def toEach: T = to(Each)
  def toDozen: T = to(Dozen)
  def toScore: T = to(Score)
  def toGross: T = to(Gross)
}

/**
  * Base trait for units of [[org.terra.Dimensionless]]
  *
  * The DimensionlessUnit is a useful paradox
  */
trait DimensionlessUnit[C <: TypeContext] 
    extends UnitOfMeasure[DimensionlessLike[C], C#T, C] 
    with UnitConverter[C#T, C] {

  type Dimensionless = DimensionlessLike[C]

  def apply(t: C#T)(
    implicit tag: ClassTag[C#T], ops: TerraOps[C]): Dimensionless =
    new Dimensionless(t, this)
}

trait DimensionlessOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]
  implicit val tag = ops.getClassTagT

  type Dimensionless = DimensionlessLike[C]

  trait DimensionlessUnitT extends DimensionlessUnit[C]

  object Dimensionless extends Dimension[DimensionlessLike[C], C#T, C] {

    def apply[A](a: A, unit: DimensionlessUnit[C])(
      implicit num: Numeric[A]): Dimensionless =
      new Dimensionless(ops.convDouble(num.toDouble(a)), unit)
    def apply(t: C#T, unit: DimensionlessUnit[C]): Dimensionless =
      new Dimensionless(t, unit)
    def apply(value: Any) = parse(value)
    def name = "Dimensionless"
    def primaryUnit = Each
    def siUnit = Each
    def units = Set(Each, Percent, Dozen, Score, Gross)
  }

  /**
    * Represents a unit of singles
    */
  object Each extends DimensionlessUnitT with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = "ea"
  }

  /**
    * Represents a number of hundredths (0.01)
    */
  object Percent extends DimensionlessUnitT {
    val conversionFactor: Double = 1e-2
    val symbol = "%"
  }

  /**
    * Represents a unit of dozen (12)
    */
  object Dozen extends DimensionlessUnitT {
    val conversionFactor = 12d
    val symbol = "dz"
  }

  /**
    * Represents a unit of scores (20)
    */
  object Score extends DimensionlessUnitT {
    val conversionFactor = 20d
    val symbol = "score"
  }

  /**
    * Represents a unit of gross (144)
    */
  object Gross extends DimensionlessUnitT {
    val conversionFactor = 144d
    val symbol = "gr"
  }

  object DimensionlessConversions {

    lazy val percent = Percent(1)
    lazy val each = Each(1)
    lazy val dozen = Dozen(1)
    lazy val score = Score(1)
    lazy val gross = Gross(1)
    lazy val hundred = Each(1e2)
    lazy val thousand = Each(1e3)
    lazy val million = Each(1e6)

    import scala.language.implicitConversions

    implicit class DimensionlessConversions[A](a: A)(implicit n: Numeric[A]) {
      def percent = Percent(a)
      def each = Each(a)
      def ea = Each(a)
      def dozen = Dozen(a)
      def dz = Dozen(a)
      def score = Score(a)
      def gross = Gross(a)
      def gr = Gross(a)
      def hundred = Each(ops.convDouble(n.toDouble(a) * 1e2))
      def thousand = Each(ops.convDouble(n.toDouble(a) * 1e3))
      def million = Each(ops.convDouble(n.toDouble(a) * 1e6))
    }
    /**
      * Provides an implicit conversion from Dimensionless to Double, allowing a Dimensionless value
      * to be used anywhere a Double (or similar primitive) is required
      *
      * @param d Dimensionless
      * @return
      */
    implicit def dimensionlessToT(d: Dimensionless): Double = 
      ops.num.toDouble(d.toEach)
  }
}

