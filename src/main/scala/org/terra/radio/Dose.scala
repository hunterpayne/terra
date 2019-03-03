/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package radio

import org.terra.mass.MassLike
import org.terra.energy.EnergyLike
import org.terra.time.TimeLike

/**
 * Its important to note that while Dose and AbsorbedDose are simliar 
 * measures (both are energy/mass dimensions), they are decidedly different
 * and no conversions between the two dimensions should be directly possible
 * through implicit conversions EVER.  This is because AbsorbedDose is used to
 * measure absorbed dose which just measures energy deposited into a mass of
 * material while Dose is used to measure equivalent/effective/committed doses
 * which are measures of the damage done to biological tissues.  Since this
 * is an easy and disasterous mistake to make, it is critical that Terra
 * doesn't allow any sort of magic conversions that allow this mistake.
 * @author  Hunter Payne
 *
 * @param value value in [[org.terra.radio.Sievert]]
 */
final class DoseLike[C <: TypeContext](val value: C#T, val unit: DoseUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[DoseLike[C], C#T, C] {

  import ops.doseOps._
  import ops.energyOps.Joules

  type Mass = MassLike[C]
  type Energy = EnergyLike[C]
  type Time = TimeLike[C]

  def dimension: Dimension[DoseLike[C], C#T, C] = Dose

  def *(that: Mass): Energy = {
    implicit val opsArg = ops
    Joules(ops.num.times(this.toSieverts, that.toKilograms))
  }

  def toSieverts = to(Sieverts)
  def toRems = to(Rems)
}

trait DoseUnit[C <: TypeContext] extends UnitOfMeasure[DoseLike[C], C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) = new DoseLike[C](t, this)
}

trait DoseOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]

  trait DoseUnitT extends DoseUnit[C]

  object Dose extends Dimension[DoseLike[C], C#T, C] {
    private[radio] def apply[A](a: A, unit: DoseUnit[C])(
      implicit n: Numeric[A]) = 
      new DoseLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "Dose"
    def primaryUnit = Sieverts
    def siUnit = Sieverts
    def units = Set(Sieverts, Rems)
  }

  object Rems extends DoseUnitT with UnitConverter[C#T, C] {
    val symbol = "rem"
    val conversionFactor = 0.01
  }

  object Sieverts extends DoseUnitT with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = "Sv"
  }

  object DoseConversions {
    lazy val sievert = Sieverts(1)
    lazy val rem = Rems(1)

    implicit class DoseConversions[A](a: A)(implicit n: Numeric[A]) {
      def sieverts = Sieverts(a)
      def rems = Rems(a)
    }
  }
}


