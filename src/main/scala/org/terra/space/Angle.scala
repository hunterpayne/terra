/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package space

import org.terra.motion.AngularVelocityLike
import org.terra.time.{ TimeLike, TimeIntegral }

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value value in [[org.terra.space.Radians]]
 */
final class AngleLike[C <: TypeContext](val value: C#T, val unit: AngleUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[AngleLike[C], C#T, C] 
    with TimeIntegral[AngularVelocityLike[C], C#T, C#T, C] {

  import ops.angleOps._
  import ops.timeOps.Seconds
  import ops.angularVelocityOps.RadiansPerSecond
  import ops.timeOps.Seconds

  type Length = LengthLike[C]
  type AngularVelocity = AngularVelocityLike[C]

  def dimension: Dimension[AngleLike[C], C#T, C] = Angle

  def toRadians = to(Radians)
  def toDegrees = to(Degrees)
  def toGradians = to(Gradians)
  def toTurns = to(Turns)
  def toArcminutes = to(Arcminutes)
  def toArcseconds = to(Arcseconds)

  def sin = {
    implicit val e: HasEnsureType[C#T] = makeEnsureType
    ops.sinT[C#T](toRadians)
  }
  def cos = {
    implicit val e: HasEnsureType[C#T] = makeEnsureType
    ops.cosT[C#T](toRadians)
  }
  def tan = {
    implicit val e: HasEnsureType[C#T] = makeEnsureType
    ops.tanT[C#T](toRadians)
  }
  def asin = {
    implicit val e: HasEnsureType[C#T] = makeEnsureType
    ops.asinT[C#T](toRadians)
  }
  def acos = {
    implicit val e: HasEnsureType[C#T] = makeEnsureType
    ops.acosT[C#T](toRadians)
  }

  /**
    * length of the arc traveled by a point on the rim of a circle with this
    * angle traveled and the given (constant) radius from the center of
    * rotation
    * @param radius the distance from the center of rotation
    * @return arc length with given arc measure and radius
    */
  def onRadius(radius: Length)(implicit ops: TerraOps[C]): Length = 
    radius * toRadians

  protected def timeDerived: AngularVelocity = {
    implicit val opsArg = ops
    RadiansPerSecond(toRadians)
  }

  override protected def time: Time = {
    implicit val opsArg = ops
    Seconds(1)
  }
}

trait AngleUnit[C <: TypeContext] extends UnitOfMeasure[AngleLike[C], C#T, C]
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) = new AngleLike[C](t, this)
}

trait AngleOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]

  trait AngleUnitT extends AngleUnit[C]

  object Angle extends Dimension[AngleLike[C], C#T, C] {
    private[space] def apply[A](a: A, unit: AngleUnit[C])(
      implicit n: Numeric[A], ops: TerraOps[C]) = 
      new AngleLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "Angle"
    def primaryUnit = Radians
    def siUnit = Radians
    def units = Set(Radians, Degrees, Gradians, Turns, Arcminutes, Arcseconds)
  }

  object Radians extends AngleUnitT with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = "rad"
  }

  object Degrees extends AngleUnitT {
    val symbol = "°"
    val conversionFactor = math.Pi / 180d
  }

  object Gradians extends AngleUnitT {
    val symbol = "grad"
    val conversionFactor = Turns.conversionFactor / 400d
  }

  object Turns extends AngleUnitT {
    val symbol = "turns"
    val conversionFactor = 2 * math.Pi
  }

  object Arcminutes extends AngleUnitT {
    val symbol = "amin"
    val conversionFactor = math.Pi / 10800d
  }

  object Arcseconds extends AngleUnitT {
    val symbol = "asec"
    val conversionFactor = 
      1d / ops.timeOps.Time.SecondsPerMinute * Arcminutes.conversionFactor
  }

  object AngleConversions {
    lazy val radian = Radians(1)
    lazy val degree = Degrees(1)
    lazy val gradian = Gradians(1)
    lazy val turn = Turns(1)
    lazy val arcminute = Arcminutes(1)
    lazy val arcsecond = Arcseconds(1)

    implicit class AngleConversions[A](a: A)(implicit n: Numeric[A]) {
      def radians = Radians(a)
      def degrees = Degrees(a)
      def gradians = Gradians(a)
      def turns = Turns(a)
      def arcminutes = Arcminutes(a)
      def arcseconds = Arcseconds(a)
    }
  }
}

