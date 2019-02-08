/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package electro

import scala.reflect.ClassTag

import space.AreaLike

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value value in [[org.terra.electro.Teslas]]
 */
final class MagneticFluxDensityLike[C <: TypeContext](
  val value: C#T, val unit: MagneticFluxDensityUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[MagneticFluxDensityLike[C], C#T, C] {

  import ops.magneticFluxDensityOps._
  import ops.magneticFluxOps.Webers

  type Area = AreaLike[C]
  type MagneticFlux = MagneticFluxLike[C]

  def dimension: Dimension[MagneticFluxDensityLike[C], C#T, C] = 
    MagneticFluxDensity

  def *(that: Area)(implicit ops: TerraOps[C]): MagneticFlux =
    Webers(ops.num.times(this.toTeslas, that.toSquareMeters))

  def toTeslas = to(Teslas)
  def toGuass = to(Gauss)
}

trait MagneticFluxDensityUnit[C <: TypeContext] 
    extends UnitOfMeasure[MagneticFluxDensityLike[C], C#T, C]
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) = 
    new MagneticFluxDensityLike[C](t, this)
}

trait MagneticFluxDensityOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]

  trait MagneticFluxDensityUnitT extends MagneticFluxDensityUnit[C]

  object MagneticFluxDensity 
      extends Dimension[MagneticFluxDensityLike[C], C#T, C] {
    private[electro] def apply[A](a: A, unit: MagneticFluxDensityUnit[C])(
      implicit n: Numeric[A]) = 
      new MagneticFluxDensityLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "MagneticFluxDensity"
    def primaryUnit = Teslas
    def siUnit = Teslas
    def units = Set(Teslas, Gauss)
  }

  object Teslas extends MagneticFluxDensityUnitT with PrimaryUnit[C#T, C] 
      with SiUnit {
    val symbol = "T"
  }

  object Gauss extends MagneticFluxDensityUnitT {
    val conversionFactor = 100 * MetricSystem.Micro
    val symbol = "Gs"
  }

  object MagneticFluxDensityConversions {
    lazy val tesla = Teslas(1)
    lazy val gauss = Gauss(1)

    implicit class MagneticFluxDensityConversions[A](a: A)(
      implicit num: Numeric[A]) {
      def teslas = Teslas(a)
      def gauss = Gauss(a)
    }
  }
}

