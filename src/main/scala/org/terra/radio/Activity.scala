/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2018, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package radio

import scala.reflect.ClassTag

/**
 * @author  Hunter Payne
 *
 * @param value Double
 */
final class ActivityLike[C <: TypeContext](
  val value: C#T, val unit: ActivityUnit[C])(implicit ops: TerraOps[C])
    extends Quantity[ActivityLike[C], C#T, C] {

  import ops.activityOps._
  import ops.particleFluxOps.BecquerelsPerSquareMeterSecond

  type AreaTime = AreaTimeLike[C]
  type ParticleFlux = ParticleFluxLike[C]

  def dimension: Dimension[ActivityLike[C], C#T, C] = Activity

  def /(that: AreaTime)(implicit ops: TerraOps[C]): ParticleFlux = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    BecquerelsPerSquareMeterSecond(
      ops.div[C#T](this.toBecquerels, that.toSquareMeterSeconds))
  }

  def toCuries = to(Curies)
  def toBecquerels = to(Becquerels)
  def toRutherfords = to(Rutherfords)
}

trait ActivityUnit[C <: TypeContext]
    extends UnitOfMeasure[ActivityLike[C], C#T, C] with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) = 
    new ActivityLike[C](t, this)
}

trait ActivityOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]
  def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait ActivityUnitT extends ActivityUnit[C]

  object Activity extends Dimension[ActivityLike[C], C#T, C] {
    private[radio] def apply[A](a: A, unit: ActivityUnit[C])(
      implicit n: Numeric[A]) =
      new ActivityLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "Activity"
    def primaryUnit = Becquerels
    def siUnit = Becquerels
    def units = Set(Becquerels, Curies, Rutherfords)
  }

  object Curies extends ActivityUnitT {
    val conversionFactor = 3.7 * Math.pow(10, 10)
    val symbol = "Ci"
  }

  object Rutherfords extends ActivityUnitT {
    val conversionFactor = 1000000.0
    val symbol = "Rd"
  }

  object Becquerels extends ActivityUnitT with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = "Bq"
  }

  object ActivityConversions {
    lazy val curie = Curies(1)
    lazy val rutherford = Rutherfords(1)
    lazy val becquerel = Becquerels(1)

    implicit class ActivityConversions[A](a: A)(implicit n: Numeric[A]) {
      def curies = Curies(a)
      def rutherfords = Rutherfords(a)
      def becquerels = Becquerels(a)
    }
  }
}
