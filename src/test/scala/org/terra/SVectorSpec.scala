/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra

import org.scalatest.{FlatSpec, Matchers}

import org.terra.space.{ LengthLike, AreaLike, VolumeLike, AngleLike }
import standard._
import standard.space.{ Degrees, Kilometers, SquareKilometers, SquareMeters, Meters }

/**
 * @author  garyKeorkunian
 * @since   0.3.0
 *
 */
class SVectorSpec extends FlatSpec with Matchers {

  type Tuple = StandardTuple
  type Length = LengthLike[Tuple]
  type Area = AreaLike[Tuple]
  type Volume = VolumeLike[Tuple]
  type Angle = AngleLike[Tuple]
  // TODO Expand tests

  behavior of "ValueVector"

  it should "create a Vector with expected values from Cartesian coordinates" in {
    val vector = SVector[Tuple](1, 10, 5)
    vector.coordinates(0) should be(1)
    vector.coordinates(1) should be(10)
    vector.coordinates(2) should be(5)
  }

  it should "create a Vector with expected values from Polar coordinates" in {
    val vector = SVector[Tuple](10d, Degrees(45d))
    vector.magnitude should be(10d)
    vector.angle2() should be(Degrees(45d))
  }

  it should "equate to like Vectors" in {
    val x = 1d
    val y = 10d
    val z = 5d
    SVector[Tuple](x, y, z).equals(SVector[Tuple](x, y, z)) should be(right = true)
    SVector[Tuple](x, y, z) == SVector[Tuple](x, y, z) should be(right = true)
  }

  it should "not equate to dislike Vectors" in {
    val x = 1d
    val y = 10d
    val z = 5d
    SVector[Tuple](x, y, z).equals(SVector[Tuple](z, y, z)) should be(right = false)
    SVector[Tuple](x, y, z) != SVector[Tuple](z, y, z) should be(right = true)
  }

  it should "calculate the magnitude" in {
    val vector = SVector[Tuple](3, 4, 5)
    vector.magnitude should be(math.sqrt(3 * 3 + 4 * 4 + 5 * 5))
  }

  it should "calculate the polar angle" in {
    val vector = SVector[Tuple](3d, 4d)
    vector.angle2().toRadians should be(math.atan2(4, 3))
  }

  it should "calculate polar coordinates" in {
    val (r, theta) = SVector[Tuple](3, 4).polar2()
    r should be(5)
    theta.toRadians should be(math.atan2(4, 3))
  }

  it should "normalize a Vector" in {
    val x = 3d
    val y = 4d
    val z = 5d
    val normalized = SVector[Tuple](x, y, z).normalize
    normalized.magnitude should be(1.0 +- 0.0000000000000001)
  }

  it should "map to another ValueVector" in {
    val v = SVector[Tuple](1, 2, 3)
    val squared = v.map[Double](Math.pow(_, 2))
    squared.equals(SVector[Tuple](1, 4, 9)) should be(right = true)
  }

  it should "map to QuantityVector" in {
    val v = SVector[Tuple](1d, 2d, 3d)
    val lengthVector = v.map[Length](Meters.apply)
    lengthVector.equals(SVector[Length, Double, Tuple](Meters(1d), Meters(2d), Meters(3d))) should be(right = true)
  }

  it should "add two Vectors" in {
    val x = 1d
    val y = 2d
    val z = 3d
    val a = 5d
    SVector[Tuple](x, y, z).plus(SVector[Tuple](a, a, a)) should be(SVector[Tuple](x + a, y + a, z + a))
    SVector[Tuple](x, y, z) + SVector[Tuple](a, a, a) should be(SVector[Tuple](x + a, y + a, z + a))
  }

  it should "subtract two Vectors" in {
    val x = 1d
    val y = 2d
    val z = 3d
    val a = 5d
    SVector[Tuple](x, y, z).minus(SVector[Tuple](a, a, a)) should be(SVector[Tuple](x - a, y - a, z - a))
    SVector[Tuple](x, y, z) - SVector[Tuple](a, a, a) should be(SVector[Tuple](x - a, y - a, z - a))
  }

  it should "rescale a Vector" in {
    val x = 1d
    val y = 2d
    val z = 3d
    val r = 5d
    SVector[Tuple](x, y, z).times(r) should be(SVector[Tuple](x * r, y * r, z * r))
    SVector[Tuple](x, y, z) * r should be(SVector[Tuple](x * r, y * r, z * r))
    SVector[Tuple](x, y, z).divide(r) should be(SVector[Tuple](x / r, y / r, z / r))
    SVector[Tuple](x, y, z) / r should be(SVector[Tuple](x / r, y / r, z / r))
  }

  it should "dot product with another ValueVector" in {
    val x = 1d
    val y = 2d
    val z = 3d
    val a = 5d
    val expRes = x * a + y * a + z * a
    SVector[Tuple](x, y, z).dotProduct(SVector[Tuple](a, a, a)) should be(expRes)
    SVector[Tuple](x, y, z) * SVector[Tuple](a, a, a) should be(expRes)
  }

  it should "dot product with a QuantityVector" in {
    val dVector = SVector[Tuple](1d, 2d, 3d)
    val qVector = SVector[Length, Double, Tuple](Meters(1d), Meters(2d), Meters(3d))
    dVector dotProduct qVector should be(qVector dotProduct dVector)
  }

  it should "cross product with a ValueVector (with 3 coordinates each)" in {
    val x = 1d
    val y = 2d
    val z = 3d
    val a = 5d
    val expRes = SVector[Tuple](y * a - z * a, z * a - x * a, x * a - y * a)
    SVector[Tuple](x, y, z).crossProduct(SVector[Tuple](a, a, a)) should be(expRes)
    SVector[Tuple](x, y, z) #* SVector[Tuple](a, a, a) should be(expRes)

    val up = SVector[Tuple](1d, 2d, 3d)
    val left = SVector[Tuple](3d, 2d, 1d)
    val forward = up crossProduct left
    val back = left crossProduct up

    forward should be (SVector[Tuple](-4.0, 8.0, -4.0))
    back should be (SVector[Tuple](4.0, -8.0, 4.0))
  }

  it should "crossProduct with a QuantityVector" in {
    val dVector = SVector[Tuple](1d, 2d, 3d)
    val qVector = SVector[Length, Double, Tuple](Meters(1d), Meters(2d), Meters(3d))
    dVector crossProduct qVector should be(qVector crossProduct dVector)
  }

  it should "throw an exception on crossProduct two Vectors with 7 dimensions" in {
    val v1 = SVector[Tuple](1d, 2d, 3d, 4d, 5d, 6d, 7d)
    val v2 = SVector[Tuple](1d, 2d, 3d, 4d, 5d, 6d, 7d)
    intercept[UnsupportedOperationException] {
      v1 crossProduct v2
    }
  }

  it should "throw an exception on crossProduct of arbitrary size" in {
    val vector3 = SVector[Tuple](1d, 2d, 3d)
    val vector4 = SVector[Tuple](1d, 2d, 3d, 4d)
    val vector7 = SVector[Tuple](1d, 2d, 3d, 5d, 6d, 7d)

    intercept[UnsupportedOperationException] {
      vector3 crossProduct vector4
    }
    intercept[UnsupportedOperationException] {
      vector4 crossProduct vector3
    }

    intercept[UnsupportedOperationException] {
      vector4 crossProduct vector4
    }

    intercept[UnsupportedOperationException] {
      vector7 crossProduct vector4
    }
    intercept[UnsupportedOperationException] {
      vector4 crossProduct vector7
    }
  }


  behavior of "QuantityVector"

  it should "create a Vector with expected values" in {
    val vector = SVector[Length, Double, Tuple](
      Kilometers(1d), Kilometers(10d), Kilometers(5d))
    vector.coordinates(0) should be(Kilometers(1d))
    vector.coordinates(1) should be(Kilometers(10d))
    vector.coordinates(2) should be(Kilometers(5d))
  }

  it should "create a Vector with expected values from Polar coordinates" in {
    val vector = SVector(Kilometers(10d), Degrees(45d))
    vector.magnitude should be(Kilometers(10d))
    vector.angle2() should be(Degrees(45d))
  }

  it should "equate to like Vectors" in {
    val x = Kilometers(1d)
    val y = Kilometers(2d)
    val z = Kilometers(3d)
    SVector[Length, Double, Tuple](x, y, z).equals(SVector[Length, Double, Tuple](x, y, z)) should be(right = true)
    SVector[Length, Double, Tuple](x, y, z) == SVector[Length, Double, Tuple](x, y, z) should be(right = true)
  }

  it should "not equate to dislike Vectors" in {
    val x = Kilometers(1d)
    val y = Kilometers(2d)
    val z = Kilometers(3d)
    SVector[Length, Double, Tuple](x, y, z).equals(SVector[Length, Double, Tuple](z, y, z)) should be(right = false)
    SVector[Length, Double, Tuple](x, y, z) != SVector[Length, Double, Tuple](z, y, z) should be(right = true)
  }

  it should "determine a magnitude" in {
    val vector = SVector[Length, Double, Tuple](Kilometers(.003), Kilometers(.004), Kilometers(.005))
    vector.magnitude should be(Meters(math.sqrt(3 * 3 + 4 * 4 + 5 * 5)))
  }

  it should "calculate the polar angle" in {
    val vector = SVector[Length, Double, Tuple](Meters(3d), Meters(4d))
    vector.angle2().toRadians should be(math.atan2(4, 3))
  }

  it should "calculate polar coordinates" in {
    val (r, theta) = SVector[Length, Double, Tuple](Meters(3), Meters(4)).polar2()
    r should be(Meters(5))
    theta.toRadians should be(math.atan2(4, 3))
  }

  it should "normalize a Vector" in {
    implicit val tol = Kilometers(1e-15)
    val x = Kilometers(3)
    val y = Kilometers(4)
    val z = Kilometers(5)
    val normalized = SVector[Length, Double, Tuple](x, y, z).normalize(Kilometers)
    normalized.magnitude =~ Kilometers(1.0) should be(right = true)
  }

  it should "map to a ValueVector" in {
    val x = Kilometers(1)
    val y = Kilometers(2)
    val z = Kilometers(3)
    val quantityVector = SVector[Length, Double, Tuple](x, y, z)
    quantityVector.map[Double](_.to(Kilometers)).equals(SVector[Tuple](1, 2, 3)) should be(right = true)
    quantityVector.to(Kilometers).equals(SVector[Tuple](1, 2, 3)) should be(right = true)
    quantityVector.map[Double](_.to(Meters)).equals(SVector[Tuple](1000, 2000, 3000)) should be(right = true)
  }

  it should "map to a QuantityVector" in {
    val x = Kilometers(1)
    val y = Kilometers(2)
    val z = Kilometers(3)
    val quantityVector = SVector[Length, Double, Tuple](x, y, z)
    quantityVector.map[Area](l ⇒ l * l).equals(SVector[Area, Double, Tuple](SquareKilometers(1), SquareKilometers(4), SquareKilometers(9))) should be(right = true)
    quantityVector.map[Length](_ * 2).equals(SVector[Length, Double, Tuple](Kilometers(2), Kilometers(4), Kilometers(6))) should be(right = true)
  }

  it should "add two Vectors" in {
    val x = Kilometers(1)
    val y = Kilometers(2)
    val z = Kilometers(3)
    val a = Kilometers(5)
    SVector[Length, Double, Tuple](x, y, z).plus(SVector[Length, Double, Tuple](a, a, a)) should be(SVector[Length, Double, Tuple](x + a, y + a, z + a))
  }

  it should "subtract two Vectors" in {
    val x = Kilometers(1)
    val y = Kilometers(2)
    val z = Kilometers(3)
    val a = Kilometers(5)
    SVector[Length, Double, Tuple](x, y, z).minus(SVector[Length, Double, Tuple](a, a, a)) should be(SVector[Length, Double, Tuple](x - a, y - a, z - a))
  }

  it should "rescale a Vector" in {
    val x = Kilometers(1)
    val y = Kilometers(2)
    val z = Kilometers(3)
    val r = 5d
    SVector[Length, Double, Tuple](x, y, z).times(r) should be(SVector[Length, Double, Tuple](x * r, y * r, z * r))
    SVector[Length, Double, Tuple](x, y, z) * r should be(SVector[Length, Double, Tuple](x * r, y * r, z * r))
    SVector[Length, Double, Tuple](x, y, z).divide(r) should be(SVector[Length, Double, Tuple](x / r, y / r, z / r))
    SVector[Length, Double, Tuple](x, y, z) / r should be(SVector[Length, Double, Tuple](x / r, y / r, z / r))
  }

  it should "times and divide with another Quantity" in {
    val x = SquareMeters(1)
    val y = SquareMeters(2)
    val z = SquareMeters(3)
    val r = Meters(5)
    val rs = SquareMeters(.5)
    SVector[Area, Double, Tuple](x, y, z).times(_ * r) should be(SVector[Volume, Double, Tuple](x * r, y * r, z * r))
    SVector[Area, Double, Tuple](x, y, z).divide(_ / r) should be(SVector[Length, Double, Tuple](x / r, y / r, z / r))

    SVector[Area, Double, Tuple](x, y, z).divide(rs) should be(SVector[Tuple](x / rs, y / rs, z / rs))
    SVector[Area, Double, Tuple](x, y, z) / rs should be(SVector[Tuple](x / rs, y / rs, z / rs))
  }

  it should "dot product with a ValueVector" in {
    val x = Kilometers(1)
    val y = Kilometers(2)
    val z = Kilometers(3)
    val a = 5d
    val expRes = x * a + y * a + z * a
    SVector[Length, Double, Tuple](x, y, z).dotProduct(SVector[Tuple](a, a, a)) should be(expRes)
    SVector[Length, Double, Tuple](x, y, z) * SVector[Tuple](a, a, a) should be(expRes)
  }

  it should "dot product with another QuantityVector" in {
    import standard.space.AreaConversions._
    val x = Kilometers(1)
    val y = Kilometers(2)
    val z = Kilometers(3)
    val expRes = x * x + y * y + z * z
    SVector[Length, Double, Tuple](x, y, z).dotProduct[Length, Area](SVector(x, y, z), _ * _) should be(expRes)
  }

  it should "cross product with ValueVector (3 coordinates each)" in {
    val x = Kilometers(1)
    val y = Kilometers(2)
    val z = Kilometers(3)
    val a = 5d
    val expRes = SVector[Length, Double, Tuple](
      Kilometers(y.to(Kilometers) * a - z.to(Kilometers) * a),
      Kilometers(z.to(Kilometers) * a - x.to(Kilometers) * a),
      Kilometers(x.to(Kilometers) * a - y.to(Kilometers) * a))
    SVector[Length, Double, Tuple](x, y, z).crossProduct(SVector[Tuple](a, a, a)) should be(expRes)
    SVector[Length, Double, Tuple](x, y, z) #* SVector[Tuple](a, a, a) should be(expRes)

    val up = SVector[Length, Double, Tuple](Kilometers(1), Kilometers(2), Kilometers(3))
    val left = SVector[Tuple](3d, 2d, 1d)
    val forward = up crossProduct left
    val back = left crossProduct up

    forward should be (QuantityVector[Length, Double, Tuple](Kilometers(-4), Kilometers(8), Kilometers(-4)))
    back should be (QuantityVector[Length, Double, Tuple](Kilometers(-4), Kilometers(8), Kilometers(-4)))
  }

  it should "cross product with another QuantityVector (3 coordinates each)" in {
    import standard.space.AreaConversions._
    val x = Kilometers(1)
    val y = Kilometers(2)
    val z = Kilometers(3)
    val a = Kilometers(5)
    val expRes = SVector[Area, Double, Tuple](
      y * a - z * a,
      z * a - x * a,
      x * a - y * a)
    SVector[Length, Double, Tuple](x, y, z).crossProduct[Length, Area](SVector[Length, Double, Tuple](a, a, a), _ * _) should be(expRes)

    val up = SVector[Length, Double, Tuple](Kilometers(1), Kilometers(2), Kilometers(3))
    val left = SVector[Tuple](3d, 2d, 1d)
    val forward = up crossProduct left
    val back = left crossProduct up

    forward should be (QuantityVector[Length, Double, Tuple](Kilometers(-4), Kilometers(8), Kilometers(-4)))
    back should be (QuantityVector[Length, Double, Tuple](Kilometers(-4), Kilometers(8), Kilometers(-4)))
  }

  it should "throw an exception on cross product two Vectors with 7 coordinates each" in {
    import scala.language.implicitConversions
    implicit def nToQ(d: Int) = Kilometers(d)
    val v1 = SVector[Tuple](1d, 2d, 3d, 5d, 6d, 7d)
    val v2 = SVector[Tuple](1d, 2d, 3d, 5d, 6d, 7d)
    intercept[UnsupportedOperationException] {
      v1 crossProduct v2
    }
  }

  it should "throw an exception on crossProduct of arbitrary size" in {
    import scala.language.implicitConversions
    implicit def nToQ(d: Int) = Kilometers(d)
    val qv3 = SVector[Tuple](1d, 2d, 3d)
    val qv4 = SVector[Tuple](1d, 2d, 3d, 4d)
    val qv7 = SVector[Tuple](1d, 2d, 3d, 5d, 6d, 7d)
    val dv3 = SVector[Tuple](1d, 2d, 3d)
    val dv4 = SVector[Tuple](1d, 2d, 3d, 4d)
    val dv7 = SVector[Tuple](1d, 2d, 3d, 5d, 6d, 7d)

    // No crossProduct 3D with other sized vectors
    intercept[UnsupportedOperationException] {
      qv3 crossProduct dv7
    }
    intercept[UnsupportedOperationException] {
      qv3 crossProduct dv4
    }
    intercept[UnsupportedOperationException] {
      dv7 crossProduct qv3
    }
    intercept[UnsupportedOperationException] {
      dv4 crossProduct qv3
    }

    // NO crossProduct 7D with other sized vectors
    intercept[UnsupportedOperationException] {
      qv7 crossProduct dv3
    }
    intercept[UnsupportedOperationException] {
      qv7 crossProduct dv4
    }
    intercept[UnsupportedOperationException] {
      dv3 crossProduct qv7
    }
    intercept[UnsupportedOperationException] {
      dv4 crossProduct qv7
    }

    // No crossProduct with other matching size vectors
    intercept[UnsupportedOperationException] {
      dv4 crossProduct qv4
    }
    intercept[UnsupportedOperationException] {
      qv4 crossProduct dv4
    }
  }



 it should "convert to a ValueVector based on a Unit" in {
    val x = Kilometers(1)
    val y = Kilometers(2)
    val z = Kilometers(3)
    val quantityVector = SVector[Length, Double, Tuple](x, y, z)
    quantityVector.to(Kilometers).equals(SVector[Tuple](1d, 2d, 3d)) should be(right = true)
    quantityVector.to(Meters).equals(SVector[Tuple](1000d, 2000d, 3000d)) should be(right = true)
  }

  it should "convert to a QuantityVector of the same dimension in the supplied Unit" in {
    val x = Kilometers(1)
    val y = Kilometers(2)
    val z = Kilometers(3)
    val quantityVector = SVector[Length, Double, Tuple](x, y, z)
    quantityVector.in(Meters).equals(SVector[Length, Double, Tuple](Meters(1000d), Meters(2000d), Meters(3000d))) should be(right = true)
  }
}
