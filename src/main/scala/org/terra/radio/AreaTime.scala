/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package radio

import org.terra.space.AreaLike
import org.terra.time.TimeLike

/**
 * @author  Hunter Payne
 *
 * @param value Double
 */
final class AreaTimeLike[C <: TypeContext](
  val value: C#T, val unit: AreaTimeUnit[C])(implicit ops: TerraOps[C])
    extends Quantity[AreaTimeLike[C], C#T, C] {

  import ops.areaTimeOps._
  import ops.timeOps.Seconds
  import ops.areaOps.SquareMeters

  type Area = AreaLike[C]
  type Time = TimeLike[C]

  def dimension: Dimension[AreaTimeLike[C], C#T, C] = AreaTime

  def /(that: Area)(implicit ops: TerraOps[C]): Time = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    implicit val num: Numeric[C#T] = ops.num
    Seconds(ops.div[C#T](this.toSquareMeterSeconds, that.toSquareMeters))
  }
  def /(that: Time)(implicit ops: TerraOps[C]): Area = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    SquareMeters(ops.div[C#T](
      this.toSquareMeterSeconds, ops.rconvT(that.toSeconds)))
  }

  def toSquareMeterSeconds = to(SquareMeterSeconds)
  def toSquareCentimeterSeconds = to(SquareCentimeterSeconds)
}

trait AreaTimeUnit[C <: TypeContext] 
    extends UnitOfMeasure[AreaTimeLike[C], C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) = new AreaTimeLike[C](t, this)
}

trait AreaTimeOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]

  trait AreaTimeUnitT extends AreaTimeUnit[C]

  /**
    * Factory singleton for [[org.terra.radio.AreaTime]] values
    */
  object AreaTime extends Dimension[AreaTimeLike[C], C#T, C] {
    private[radio] def apply[A](a: A, unit: AreaTimeUnit[C])(
      implicit n: Numeric[A]) =
      new AreaTimeLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(area: AreaLike[C], time: TimeLike[C])(
      implicit ops: TerraOps[C]): AreaTimeLike[C] = {
      implicit val tag = ops.getClassTagT
      SquareMeterSeconds(ops.num.times(
        area.toSquareMeters, ops.rconvT(time.toSeconds)))
    }
    def apply(value: Any) = parse(value)
    def name = "AreaTime"
    def primaryUnit = SquareMeterSeconds
    def siUnit = SquareMeterSeconds
    def units = Set(SquareMeterSeconds, SquareCentimeterSeconds)
  }

  object SquareMeterSeconds extends AreaTimeUnitT with PrimaryUnit[C#T, C]
      with SiUnit {
    val symbol = "m²‧s"
  }

  object SquareCentimeterSeconds extends AreaTimeUnitT 
      with UnitConverter[C#T, C] {
    val symbol = "cm²‧s"
    val conversionFactor = 0.0001
  }

  object AreaTimeConversions {
    lazy val squareMeterSecond = SquareMeterSeconds(1)
    lazy val squareCentimeterSecond = SquareCentimeterSeconds(1)

    implicit class AreaTimeConversions[A](a: A)(implicit n: Numeric[A]) {
      def squareMeterSeconds = SquareMeterSeconds(a)
      def squareCentimeterSeconds = SquareCentimeterSeconds(a)
    }
  }
}

