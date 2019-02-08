/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2015-2017, Carlos Quiroz                                         **
**                                                                      **
\*                                                                      */

package org.terra
package electro

import scala.reflect.ClassTag

import space.LengthLike

/**
 * Quantity and units for Permeability
 *
 * https://en.wikipedia.org/wiki/Permeability_(electromagnetism)
 *
 * @author  cquiroz
 * @since   1.4
 *
 * @param value value in [[org.terra.electro.HenriesPerMeter]]
 */
final class PermeabilityLike[C <: TypeContext](
  val value: C#T, val unit: PermeabilityUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[PermeabilityLike[C], C#T, C] {

  import ops.permeabilityOps._
  import ops.inductanceOps.Henry

  type Length = LengthLike[C]
  type Inductance = InductanceLike[C]

  val dimension: Dimension[PermeabilityLike[C], C#T, C] = Permeability

  def *(that: Length)(implicit ops: TerraOps[C]): Inductance =
    Henry(ops.num.times(this.toHenriesPerMeter, that.toMeters))

  def toHenriesPerMeter = to(HenriesPerMeter)
  def toNewtonsPerAmpereSquared = to(NewtonsPerAmperesSquared)
}

trait PermeabilityUnit[C <: TypeContext] 
    extends UnitOfMeasure[PermeabilityLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) = 
    new PermeabilityLike[C](t, this)
}

trait PermeabilityOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]

  trait PermeabilityUnitT extends PermeabilityUnit[C]

  object Permeability extends Dimension[PermeabilityLike[C], C#T, C] {
    private[electro] def apply[A](a: A, unit: PermeabilityUnit[C])(
      implicit n: Numeric[A]) =
      new PermeabilityLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    val name = "Permeability"
    val primaryUnit = HenriesPerMeter
    val siUnit = HenriesPerMeter
    val units = Set[UnitOfMeasure[PermeabilityLike[C], C#T, C]](
      HenriesPerMeter, NewtonsPerAmperesSquared)
  }

  import ops.inductanceOps.Henry
  import ops.lengthOps.Meters
  import ops.forceOps.Newtons
  import ops.electricCurrentOps.Amperes

  object HenriesPerMeter extends PermeabilityUnitT with PrimaryUnit[C#T, C]
      with SiUnit {
    val symbol = s"${Henry.symbol}/${Meters.symbol}"
  }

  object NewtonsPerAmperesSquared extends PermeabilityUnitT 
      with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = s"${Newtons.symbol}/${Amperes.symbol}²"
  }

  object PermeabilityConversions {
    lazy val henriesPerMeter = HenriesPerMeter(1)
    lazy val newtonsPerAmperesSquared = NewtonsPerAmperesSquared(1)

    implicit class PermeabilityConversions[A](a: A)(implicit num: Numeric[A]) {
      def henriesPerMeter = HenriesPerMeter(a)
      def newtonsPerAmperesSquared = NewtonsPerAmperesSquared(a)
    }
  }
}

