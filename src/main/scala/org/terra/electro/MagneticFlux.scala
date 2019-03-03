/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package electro

import space.AreaLike
import time.TimeIntegral

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value value in [[org.terra.electro.Webers]]
 */
final class MagneticFluxLike[C <: TypeContext](
  val value: C#T, val unit: MagneticFluxUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[MagneticFluxLike[C], C#T, C]
    with TimeIntegral[ElectricPotentialLike[C], C#T, C#T, C] {

  import ops.magneticFluxOps._
  import ops.electricPotentialOps.Volts
  import ops.timeOps.Seconds
  import ops.magneticFluxDensityOps.Teslas
  import ops.areaOps.SquareMeters
  import ops.inductanceOps.Henry
  import ops.electricCurrentOps.Amperes

  type Area = AreaLike[C]
  type MagneticFluxDensity = MagneticFluxDensityLike[C]
  type Inductance = InductanceLike[C]
  type ElectricCurrent = ElectricCurrentLike[C]

  def dimension: Dimension[MagneticFluxLike[C], C#T, C] = MagneticFlux

  protected def timeDerived = Volts(toWebers)
  protected def time = Seconds(1)

  def /(that: Area)(implicit ops: TerraOps[C]): MagneticFluxDensity = {
    implicit val ensureT: HasEnsureType[C#T] = ops.converters.ensureT
    Teslas(ops.div[C#T](this.toWebers, that.toSquareMeters))
  }
  def /(that: MagneticFluxDensity)(implicit ops: TerraOps[C]): Area = {
    implicit val ensureT: HasEnsureType[C#T] = ops.converters.ensureT
    SquareMeters(ops.div[C#T](this.toWebers, that.toTeslas))
  }
  def /(that: ElectricCurrent)(implicit ops: TerraOps[C]): Inductance = {
    implicit val ensureT: HasEnsureType[C#T] = ops.converters.ensureT
    Henry(ops.div[C#T](this.toWebers, that.toAmperes))
  }
  def /(that: Inductance)(implicit ops: TerraOps[C]): ElectricCurrent = {
    implicit val ensureT: HasEnsureType[C#T] = ops.converters.ensureT
    Amperes(ops.div[C#T](this.toWebers, that.toHenry))
  }

  def toWebers = to(Webers)
}

trait MagneticFluxUnit[C <: TypeContext] 
    extends UnitOfMeasure[MagneticFluxLike[C], C#T, C]
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) = 
    new MagneticFluxLike[C](t, this)
}

trait MagneticFluxOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]

  trait MagneticFluxUnitT extends MagneticFluxUnit[C]

  object MagneticFlux extends Dimension[MagneticFluxLike[C], C#T, C] {
    private[electro] def apply[A](a: A, unit: MagneticFluxUnit[C])(
      implicit n: Numeric[A]) = 
      new MagneticFluxLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "MagneticFlux"
    def primaryUnit = Webers
    def siUnit = Webers
    def units = Set(Webers)
  }


  object Webers extends MagneticFluxUnitT with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = "Wb"
  }

  object MagneticFluxConversions {
    lazy val weber = Webers(1)

    implicit class MagneticFluxConversions[A](a: A)(implicit num: Numeric[A]) {
      def webers = Webers(a)
    }

  }
}

