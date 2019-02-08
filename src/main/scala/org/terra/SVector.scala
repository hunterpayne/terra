/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra

import scala.reflect.ClassTag

import space.{ AngleLike, AngleUnit }

/**
 * Root trait for representing Vectors
 *
 * @author garyKeorkunian
 * @since 0.3.0
 *
 * @tparam A Type for the Vector's coordinate values
 */
trait SVector[A, T, C <: TypeContext] {

  type SVectorType <: SVector[A, T, C]

  /**
   * The list of values that makeup the Vector's Cartesian coordinates
   * @return
   */
  def coordinates: Seq[A]

  /**
   * The scalar value of the Vector
   * @return
   */
  def magnitude: A

  /**
   * The angle between the two Cartesian coordinates at the supplied indices
   * @param coordinateX index of the abscissa coordinate (defaults to 0)
   * @param coordinateY index of the ordinate coordinate (defaults to 1)
   * @param unit unit for the angle (theta) component (defaults to Radians)
   * @return Angle
   */
  def angle2(coordinateX: Int = 0, coordinateY: Int = 1)(
    implicit ops: TerraOps[C]): AngleLike[C] =
    angle(coordinateX, coordinateY, ops.angleOps.Radians)
  def angle(coordinateX: Int = 0, coordinateY: Int = 1, unit: AngleUnit[C])(
    implicit ops: TerraOps[C]): AngleLike[C]

  /**
   * The polar coordinates (r, theta) of the two Cartesian coordinates at the supplied indices
   * @param coordinateX index of the abscissa coordinate (defaults to 0)
   * @param coordinateY index of the ordinate coordinate (defaults to 1)
   * @param unit unit for the angle (theta) component (defaults to Radians)
   * @return (A, Angle)
   */
  def polar2(coordinateX: Int = 0, coordinateY: Int = 1)(
    implicit ops: TerraOps[C]): (A, AngleLike[C]) =
    polar(coordinateX, coordinateY, ops.angleOps.Radians)
  def polar(coordinateX: Int = 0, coordinateY: Int = 1, unit: AngleUnit[C])(
    implicit ops: TerraOps[C]): (A, AngleLike[C]) =
    (magnitude, angle(coordinateX, coordinateY, unit))

  /**
   * Creates the Unit Vector which corresponds to this vector
   * @return
   */
  def normalize: SVectorType

  /**
   * Add two Vectors
   * @param that Vector[A]
   * @return
   */
  def plus(that: SVectorType): SVectorType
  def + = plus _

  /**
   * Subtract two Vectors
   * @param that Vector[A]
   * @return
   */
  def minus(that: SVectorType): SVectorType
  def - = minus _

  /**
   * Scale a Vector
   * @param that Double
   * @return
   */
  def times(that: T): SVectorType
  def * = times _

  /**
   * Reduce a Vector
   *
   * @param that Double
   * @return
   */
  def divide(that: T): SVectorType
  def /(that: T) = divide(that)

  /**
   * Create the Dot Product of two Vectors
   * @param that Double
   * @return
   */
  def dotProduct(that: ValueVector[T, C]): A
  def *(that: ValueVector[T, C]) = dotProduct(that)

  /**
   * Create the Cross Product of two Vectors
   * @param that Vector[A]
   * @return
   */
  def crossProduct(that: ValueVector[T, C]): SVectorType
  def #* = crossProduct _

}

object SVector {

  def apply[C <: TypeContext](coordinates: C#T*)(
    implicit tag: ClassTag[C#T], ops: TerraOps[C]): ValueVector[C#T, C] = {
    implicit val e: HasEnsureType[C#T] = ops.makeEnsureType[C#T](coordinates(0))
    new ValueVector[C#T, C](coordinates: _*)
  }

  def apply[A <: Quantity[A, T, C], T, C <: TypeContext](coordinates: A*)(
    implicit ops: TerraOps[C]): QuantityVector[A, T, C] =
    QuantityVector[A, T, C](coordinates: _*)

  /**
   * Create a 2-dimensional DoubleVector from Polar Coordinates
   * @param radius the magnitude of the vector
   * @param theta the angle from the polar axis
   * @return
   */
  def apply[C <: TypeContext](radius: C#T, theta: AngleLike[C])(
    implicit ops: TerraOps[C]): ValueVector[C#T, C] = {
    implicit val tag: ClassTag[C#T] = theta.getTag
    apply[C](
      ops.num.times(radius, theta.cos), ops.num.times(radius, theta.sin))
  }

  /**
   * Create a 2-dimensional QuantityVector[A] from Polar Coordinates
   * @param radius the magnitude of the vector
   * @param theta the angle from the polar axis
   * @tparam A Quantity type
   * @return
   */
  def apply[A <: Quantity[A, C#T, C], C <: TypeContext](
    radius: A, theta: AngleLike[C])(
    implicit ops: TerraOps[C]): QuantityVector[A, C#T, C] = {
    implicit val e: HasEnsureType[C#T] = 
      theta.makeEnsureType.asInstanceOf[HasEnsureType[C#T]]
    apply(
      radius * ops.ensureType[C#T](theta.cos),
      radius * ops.ensureType[C#T](theta.sin))
  }
}

/**
 * Value Vector
 *
 * @param coordinates T*
 */
class ValueVector[T, C <: TypeContext](val coordinates: T*)(
  implicit e: HasEnsureType[T], tag: ClassTag[T], ops: TerraOps[C])
    extends SVector[T, T, C] {

  import ops.angleOps.Radians

  implicit val n: Numeric[T] = ops.nt[T]
  type SVectorType = ValueVector[T, C]

  def magnitude: T =
    ops.sqrtT[T](coordinates.toTraversable.map(v ⇒ 
      n.times(v, v)).foldLeft(n.zero)(n.plus(_, _)))

  def angle(coordinateX: Int = 0, coordinateY: Int = 1, unit: AngleUnit[C])(
    implicit ops: TerraOps[C]): AngleLike[C] =
    Radians(ops.atan2T(coordinates(coordinateY), coordinates(coordinateX))) in unit

  def normalize: SVectorType = this.divide(magnitude)

  /**
   * Creates a ValueVector by mapping over each coordinate with the supplied function
   * @param f A => Double map function
   * @return
   */
  def map[A <: T](f: T ⇒ T): ValueVector[T, C] = 
    new ValueVector[T, C](coordinates.toTraversable.map(f).toSeq: _*)

  /**
   * Creates a QuantityVector by mapping over each coordinate with the supplied function
   * @param f Double => B
   * @tparam A <: Quantity
   * @return
   */
  def map[A <: Quantity[A, T, C]](f: T ⇒ A)(
    implicit ops: TerraOps[C]): QuantityVector[A, T, C] =
    QuantityVector[A, T, C](coordinates.toTraversable.map(f).toSeq: _*)

  def plus(that: SVectorType): SVectorType =
    new ValueVector[T, C](coordinates.toIterable.zipAll(that.coordinates.toIterable, 0d, 0d).toTraversable.map { case (t1: T, t2: T) ⇒ n.plus(t1, t2) }.toSeq: _*)

  def minus(that: SVectorType): SVectorType =
    new ValueVector[T, C](coordinates.toIterable.zipAll(that.coordinates.toIterable, 0d, 0d).toTraversable.map { case (t1: T, t2: T) ⇒ n.minus(t1, t2)}.toSeq: _*)

  def times(that: T): SVectorType = map(n.times(_, that))
  def times[A <: Quantity[A, T, C]](that: Quantity[A, T, C])(
    implicit ops: TerraOps[C]): QuantityVector[A, T, C] = 
    map[A](that * _)

  def divide(that: T): SVectorType = map(n.asInstanceOf[Fractional[T]].div(_, that))

  def dotProduct(that: SVectorType): T =
    coordinates.toIterable.zipAll(that.coordinates.toIterable, 0d, 0d).toTraversable.map { case (t1: T, t2: T) ⇒ n.times(t1, t2)}.foldLeft(n.zero)(n.plus(_, _))

  def dotProduct[A <: Quantity[A, T, C]](that: QuantityVector[A, T, C])(implicit n: Numeric[T]) = that dotProduct this

  def crossProduct(that: SVectorType) = (this.coordinates.length, that.coordinates.length) match {
    case (3, 3) ⇒
      new ValueVector[T, C](n.minus(n.times(this.coordinates(1), that.coordinates(2)), n.times(this.coordinates(2), that.coordinates(1))),
        n.minus(n.times(coordinates(2), that.coordinates(0)), n.times(this.coordinates(0), that.coordinates(2))),
        n.minus(n.times(coordinates(0), that.coordinates(1)), n.times(this.coordinates(1), that.coordinates(0))))
    case (7, 7) ⇒ throw new UnsupportedOperationException("Seven-dimensional cross product is not currently supported")
    case _      ⇒ throw new UnsupportedOperationException("Cross product is not supported on vectors with an arbitrary number of dimensions")
  }

  def crossProduct[A <: Quantity[A, T, C]](that: QuantityVector[A, T, C]): QuantityVector[A, T, C] = 
    that crossProduct this

  override def equals(obj: Any): Boolean = obj match {
    case v: ValueVector[_, _] => v.coordinates == coordinates
    case _ => false
  }

  override def hashCode(): Int = java.util.Objects.hash(coordinates)
}

/**
 * Quantity Vector
 *
 * @author garyKeorkunian
 * @since 0.3.0
 *
 * @param coordinates Variable list of A
 * @tparam A QuantityType
 */
case class QuantityVector[A <: Quantity[A, T, C], T, C <: TypeContext](
  coordinates: A*)(implicit ops: TerraOps[C]) extends SVector[A, T, C] {

  import ops.angleOps.Radians

  type SVectorType = QuantityVector[A, T, C]
  type Angle = AngleLike[C]

  implicit val tag: ClassTag[T] = coordinates(0).tag
  implicit val n: Numeric[T] = ops.nt[T]

  val valueUnit = coordinates(0).unit
  def magnitude: A = 
    coordinates.headOption match {
      case None => valueUnit(n.zero)
      case Some(head) => {
        implicit val e: HasEnsureType[T] = head.makeEnsureType
        valueUnit(ops.sqrtT[T](coordinates.toTraversable.map(v ⇒
          n.times(
            v.to(valueUnit),
            v.to(valueUnit))).foldLeft(n.zero)(n.plus(_, _))))
      }
    }

  def angle(coordinateX: Int = 0, coordinateY: Int = 1, unit: AngleUnit[C])(
    implicit ops: TerraOps[C]): Angle = {
    implicit val ensure: HasEnsureType[T] = 
      unit.makeEnsureType.asInstanceOf[HasEnsureType[T]]
    Radians(
      ops.atanT(coordinates(coordinateY) / coordinates(coordinateX))) in unit
  }

  def normalize: SVectorType = this / magnitude.to(valueUnit)

  /**
   * Creates the Unit Vector which corresponds to this vector using the given unit
   * @return
   */
  def normalize(unit: UnitOfMeasure[A, T, C]): SVectorType = 
    this / magnitude.to(unit)

  /**
   * Creates a ValueVector by mapping over each coordinate with the supplied function
   * @param f A => Double map function
   * @return
   */
  def map[B <: T](f: A ⇒ T): ValueVector[T, C] = {
    implicit val e: HasEnsureType[T] = valueUnit.makeEnsureType
    new ValueVector[T, C](coordinates.toTraversable.map(f).toSeq: _*)
  }

  /**
   * Creates a QuantityVector by mapping over each coordinate with the supplied function
   * @param f A => B
   * @tparam B <: Quantity
   * @return
   */
  def map[B <: Quantity[B, T, C]](f: A ⇒ B)(implicit ops: TerraOps[C]): 
      QuantityVector[B, T, C] = 
    QuantityVector[B, T, C](coordinates.toTraversable.map(f).toSeq: _*)

  def plus(that: SVectorType): SVectorType =
    QuantityVector[A, T, C](coordinates.toIterable.zipAll(that.coordinates.toIterable, valueUnit(0), valueUnit(0)).toTraversable.map(v ⇒ v._1 + v._2).toSeq: _*)
  def minus(that: SVectorType): SVectorType =
    QuantityVector[A, T, C](coordinates.toIterable.zipAll(that.coordinates.toIterable, valueUnit(0), valueUnit(0)).toTraversable.map(v ⇒ v._1 - v._2).toSeq: _*)

  def times(that: T): SVectorType = map(_ * that)
  def *(that: T): SVectorType = times(that)

  def times[B <: Quantity[B, T, C], C1 <: Quantity[C1, T, C]](quantTimes: A ⇒ C1): QuantityVector[C1, T, C] = map[C1](quantTimes)

  def divide(that: T): SVectorType = map[A](_ / that)
  def divide(that: A): ValueVector[T, C] = map[T](a => a / that)
  def /(that: A) = divide(that)

  def divide[C1 <: Quantity[C1, T, C]](quantDiv: A ⇒ C1)(implicit ops: TerraOps[C]): QuantityVector[C1, T, C] = map[C1](quantDiv(_))

  def dotProduct(that: ValueVector[T, C]): A =
    valueUnit(coordinates.toIterable.zipAll(that.coordinates.toIterable, valueUnit(0), 0d).toTraversable.map { case(q1: Quantity[A, T, C], t2: T) ⇒ n.times(q1.to(valueUnit), t2)}.foldLeft(n.zero)(n.plus(_, _)))

  def dotProduct[B <: Quantity[B, T, C], C1 <: Quantity[C1, T, C]](that: SVector[B, T, C], quantTimes: (A, B) ⇒ C1)(implicit nc: Numeric[C1]): C1 =
    coordinates.toIterable.zipAll(that.coordinates.toIterable, valueUnit(0), that.coordinates.head.unit(0)).toTraversable.map(v ⇒ quantTimes(v._1, v._2)).sum

  def crossProduct(that: ValueVector[T, C]): SVectorType = (this.coordinates.length, that.coordinates.length) match {
    case (3, 3) ⇒
      QuantityVector(
        (this.coordinates(1) * that.coordinates(2)) - (this.coordinates(2) * that.coordinates(1)),
        (this.coordinates(2) * that.coordinates(0)) - (this.coordinates(0) * that.coordinates(2)),
        (this.coordinates(0) * that.coordinates(1)) - (this.coordinates(1) * that.coordinates(0)))
    case (7, 7) ⇒ throw new UnsupportedOperationException("Seven-dimensional Cross Product is not currently supported")
    case _      ⇒ throw new UnsupportedOperationException("Cross Product is not supported on vectors with an arbitrary number of dimensions")
  }

  def crossProduct[B <: Quantity[B, T, C], C1 <: Quantity[C1, T, C]](that: SVector[B, T, C], quantTimes: (A, B) ⇒ C1)(implicit ops: TerraOps[C]): QuantityVector[C1, T, C] = {
    (this.coordinates.length, that.coordinates.length) match {
      case (3, 3) ⇒
        QuantityVector[C1, T, C](
          quantTimes(this.coordinates(1), that.coordinates(2)) - quantTimes(coordinates(2), that.coordinates(1)),
          quantTimes(this.coordinates(2), that.coordinates(0)) - quantTimes(coordinates(0), that.coordinates(2)),
          quantTimes(this.coordinates(0), that.coordinates(1)) - quantTimes(coordinates(1), that.coordinates(0)))
      case (7, 7) ⇒ throw new UnsupportedOperationException("Seven-dimensional Cross Product is not currently supported")
      case _      ⇒ throw new UnsupportedOperationException("Cross Product is not supported on vectors with an arbitrary number of dimensions")
    }
  }

  /**
   * Returns a ValueVector[T, C] representing the quantity values in terms of the supplied unit
   * @param uom UnitOfMeasure[A, T]
   * @return
   */
  def to(uom: UnitOfMeasure[A, T, C]): ValueVector[T, C] = this / uom(1)

  /**
   * Returns a QuantityVector with all coordinates set to the supplied unit
   * @param uom UnitOfMeasure[A, T]
   * @return
   */
  def in(uom: UnitOfMeasure[A, T, C]): QuantityVector[A, T, C] = map[A](_.in(uom))
}
