
package org.terra
package photo

/**
  * Contains all the symbols exported by the org.terra.*.photo packages.
  * Synthetic packages then subclass this trait with an object to represent
  * a package in the synthetic trees of symbols used by the unit test and
  * user's calling code.
  * @author Hunter Payne
  */
trait PhotoSymbols[Tuple <: TypeContext] {

  implicit val ops: TerraOps[Tuple]

  type Illuminance = IlluminanceLike[Tuple]
  lazy val Lux = ops.illuminanceOps.Lux

  object IlluminanceConversions {
    import ops.illuminanceOps.{ IlluminanceConversions => Convs }
    lazy val lux = Convs.lux

    implicit class IlluminanceConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.IlluminanceConversions[A](a)

    implicit object IlluminanceNumeric
        extends AbstractQuantityNumeric[IlluminanceLike[Tuple], Tuple](
      Illuminance)
  }
  val Illuminance = ops.illuminanceOps.Illuminance

  type Luminance = LuminanceLike[Tuple]
  lazy val CandelasPerSquareMeter = ops.luminanceOps.CandelasPerSquareMeter

  object LuminanceConversions {
    import ops.luminanceOps.{ LuminanceConversions => Convs }

    lazy val candelaPerSquareMeter = Convs.candelaPerSquareMeter

    implicit class LuminanceConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.LuminanceConversions[A](a)

    implicit object LuminanceNumeric
        extends AbstractQuantityNumeric[LuminanceLike[Tuple], Tuple](
      Luminance)
  }
  val Luminance = ops.luminanceOps.Luminance

  type LuminousEnergy = LuminousEnergyLike[Tuple]
  lazy val LumenSeconds = ops.luminousEnergyOps.LumenSeconds

  object LuminousEnergyConversions {
    import ops.luminousEnergyOps.{ LuminousEnergyConversions => Convs }

    lazy val lumenSecond = Convs.lumenSecond

    implicit class LuminousEnergyConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.LuminousEnergyConversions[A](a)

    implicit object LuminousEnergyNumeric
        extends AbstractQuantityNumeric[LuminousEnergyLike[Tuple], Tuple](
      LuminousEnergy)
  }
  val LuminousEnergy = ops.luminousEnergyOps.LuminousEnergy

  type LuminousExposure = LuminousExposureLike[Tuple]
  lazy val LuxSeconds = ops.luminousExposureOps.LuxSeconds

  object LuminousExposureConversions {
    import ops.luminousExposureOps.{ LuminousExposureConversions => Convs }

    lazy val luxSecond = Convs.luxSecond

    implicit class LuminousExposureConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.LuminousExposureConversions[A](a)

    implicit object LuminousExposureNumeric
        extends AbstractQuantityNumeric[LuminousExposureLike[Tuple], Tuple](
      LuminousExposure)
  }
  val LuminousExposure = ops.luminousExposureOps.LuminousExposure

  type LuminousFlux = LuminousFluxLike[Tuple]
  lazy val Lumens = ops.luminousFluxOps.Lumens

  object LuminousFluxConversions {
    import ops.luminousFluxOps.{ LuminousFluxConversions => Convs }

    lazy val lumen = Convs.lumen

    implicit class LuminousFluxConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.LuminousFluxConversions[A](a)

    implicit object LuminousFluxNumeric
        extends AbstractQuantityNumeric[LuminousFluxLike[Tuple], Tuple](
      LuminousFlux)
  }
  val LuminousFlux = ops.luminousFluxOps.LuminousFlux

  type LuminousIntensity = LuminousIntensityLike[Tuple]
  lazy val Candelas = ops.luminousIntensityOps.Candelas

  object LuminousIntensityConversions {
    import ops.luminousIntensityOps.{ LuminousIntensityConversions => Convs }

    lazy val candela = Convs.candela

    implicit class LuminousIntensityConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.LuminousIntensityConversions[A](a)

    implicit object LuminousIntensityNumeric
        extends AbstractQuantityNumeric[LuminousIntensityLike[Tuple], Tuple](
      LuminousIntensity)
  }
  val LuminousIntensity = ops.luminousIntensityOps.LuminousIntensity
}
