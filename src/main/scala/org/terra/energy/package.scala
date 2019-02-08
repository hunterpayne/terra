/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra

import scala.reflect.ClassTag

import mass.MassLike
import motion.{ VelocityLike, MomentumLike }
import energy.EnergyLike

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 */
package object energy {

  trait KineticEnergy[C <: TypeContext] {

    def apply(mass: MassLike[C], velocity: VelocityLike[C])(
      implicit ops: TerraOps[C]): EnergyLike[C] = {
      implicit val tag = ops.getClassTagT
      ops.energyOps.Joules(
        ops.num.times(
          ops.num.times(
            ops.num.times(ops.convDouble(0.5d), mass.toKilograms),
            velocity.toMetersPerSecond),
          velocity.toMetersPerSecond))
    }

    def apply(mass: MassLike[C], momentum: MomentumLike[C])(
      implicit ops: TerraOps[C]): EnergyLike[C] = {
      implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
      implicit val tag: ClassTag[C#T] = ops.getClassTagT
      ops.energyOps.Joules(
        ops.div[C#T](
          momentum.toNewtonSeconds, 
          ops.num.times(mass.toKilograms, ops.convDouble(2.0))))
    }
  }
}
