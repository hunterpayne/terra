/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package electro

import space.LengthLike

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value value in [[org.terra.electro.Henry]]
 */
final class InductanceLike[C <: TypeContext](
  val value: C#T, val unit: InductanceUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[InductanceLike[C], C#T, C] {

  import ops.inductanceOps._
  import ops.magneticFluxOps.Webers
  import ops.permeabilityOps.HenriesPerMeter

  type ElectricCurrent = ElectricCurrentLike[C]
  type MagneticFlux = MagneticFluxLike[C]
  type Length = LengthLike[C]
  type Permeability = PermeabilityLike[C]

  def dimension: Dimension[InductanceLike[C], C#T, C] = Inductance

  def *(that: ElectricCurrent)(implicit ops: TerraOps[C]): MagneticFlux =
    Webers(num.times(this.toHenry, that.toAmperes))
  def /(that: Length)(implicit ops: TerraOps[C]): Permeability = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    HenriesPerMeter(ops.div[C#T](this.toHenry, that.toMeters))
  }

  def toHenry = to(Henry)
  def toMillihenry = to(Millihenry)
  def toMicrohenry = to(Microhenry)
  def toNanohenry = to(Nanohenry)
  def toPicohenry = to(Picohenry)
}

trait InductanceUnit[C <: TypeContext] 
    extends UnitOfMeasure[InductanceLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) = 
    new InductanceLike[C](t, this)
}

trait InductanceOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]

  trait InductanceUnitT extends InductanceUnit[C] 

  object Inductance extends Dimension[InductanceLike[C], C#T, C] {
    private[electro] def apply[A](a: A, unit: InductanceUnit[C])(
      implicit n: Numeric[A]) = 
      new InductanceLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "Inductance"
    def primaryUnit = Henry
    def siUnit = Henry
    def units = Set(Henry, Millihenry, Microhenry, Nanohenry, Picohenry)
  }


  object Henry extends InductanceUnitT with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = "H"
  }

  object Millihenry extends InductanceUnitT with SiUnit {
    val symbol = "mH"
    val conversionFactor = MetricSystem.Milli
  }

  object Microhenry extends InductanceUnitT with SiUnit {
    val symbol = "μH"
    val conversionFactor = MetricSystem.Micro
  }

  object Nanohenry extends InductanceUnitT with SiUnit {
    val symbol = "nH"
    val conversionFactor = MetricSystem.Nano
  }

  object Picohenry extends InductanceUnitT with SiUnit {
    val symbol = "pH"
    val conversionFactor = MetricSystem.Pico
  }

  object InductanceConversions {
    lazy val henry = Henry(1)
    lazy val millihenry = Millihenry(1)
    lazy val microhenry = Microhenry(1)
    lazy val nanohenry = Nanohenry(1)
    lazy val picohenry = Picohenry(1)

    implicit class InductanceConversions[A](a: A)(implicit num: Numeric[A]) {
      def henry = Henry(a)
      def millihenry = Millihenry(a)
      def microhenry = Microhenry(a)
      def nanohenry = Nanohenry(a)
      def picohenry = Picohenry(a)
    }
  }
}

