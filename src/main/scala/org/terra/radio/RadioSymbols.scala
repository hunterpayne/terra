
package org.terra
package radio

/**
  * Contains all the symbols exported by the org.terra.*.radio packages.
  * Synthetic packages then subclass this trait with an object to represent
  * a package in the synthetic trees of symbols used by the unit test and
  * user's calling code.
  * @author Hunter Payne
  */
trait RadioSymbols[Tuple <: TypeContext] {

  implicit val ops: TerraOps[Tuple]

  type Activity = ActivityLike[Tuple]
  lazy val Becquerels = ops.activityOps.Becquerels
  lazy val Curies = ops.activityOps.Curies
  lazy val Rutherfords = ops.activityOps.Rutherfords

  object ActivityConversions {
    import ops.activityOps.{ ActivityConversions => Convs }

    lazy val curie = Convs.curie
    lazy val rutherford = Convs.rutherford
    lazy val becquerel = Convs.becquerel

    implicit class ActivityConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.ActivityConversions[A](a)

    implicit object ActivityNumeric
        extends AbstractQuantityNumeric[ActivityLike[Tuple], Tuple](Activity)
  }
  val Activity = ops.activityOps.Activity

  type AreaTime = AreaTimeLike[Tuple]
  lazy val SquareMeterSeconds = ops.areaTimeOps.SquareMeterSeconds
  lazy val SquareCentimeterSeconds = ops.areaTimeOps.SquareCentimeterSeconds

  object AreaTimeConversions {
    import ops.areaTimeOps.{ AreaTimeConversions => Convs }

    lazy val squareMeterSecond = Convs.squareMeterSecond
    lazy val squareCentimeterSecond = Convs.squareCentimeterSecond

    implicit class AreaTimeConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.AreaTimeConversions[A](a)

    implicit object AreaTimeNumeric
        extends AbstractQuantityNumeric[AreaTimeLike[Tuple], Tuple](AreaTime)
  }
  val AreaTime = ops.areaTimeOps.AreaTime

  type Dose = DoseLike[Tuple]
  lazy val Sieverts = ops.doseOps.Sieverts
  lazy val Rems = ops.doseOps.Rems

  object DoseConversions {
    import ops.doseOps.{ DoseConversions => Convs }

    lazy val sievert = Convs.sievert
    lazy val rem = Convs.rem
      
    implicit class DoseConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.DoseConversions[A](a)

    implicit object DoseNumeric
        extends AbstractQuantityNumeric[DoseLike[Tuple], Tuple](Dose)
  }
  val Dose = ops.doseOps.Dose

  type Irradiance = IrradianceLike[Tuple]
  lazy val WattsPerSquareMeter = ops.irradianceOps.WattsPerSquareMeter
  lazy val ErgsPerSecondPerSquareCentimeter =
    ops.irradianceOps.ErgsPerSecondPerSquareCentimeter

  object IrradianceConversions {
    import ops.irradianceOps.{ IrradianceConversions => Convs }

    lazy val wattPerSquareMeter = Convs.wattPerSquareMeter
    lazy val ergPerSecondPerSquareCentimeter =
      Convs.ergPerSecondPerSquareCentimeter
      
    implicit class IrradianceConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.IrradianceConversions[A](a)

    implicit object IrradianceNumeric
        extends AbstractQuantityNumeric[IrradianceLike[Tuple], Tuple](
      Irradiance)
  }
  val Irradiance = ops.irradianceOps.Irradiance

  type ParticleFlux = ParticleFluxLike[Tuple]
  lazy val BecquerelsPerSquareMeterSecond =
    ops.particleFluxOps.BecquerelsPerSquareMeterSecond
  lazy val BecquerelsPerSquareCentimeterSecond =
    ops.particleFluxOps.BecquerelsPerSquareCentimeterSecond

  object ParticleFluxConversions {
    import ops.particleFluxOps.{ ParticleFluxConversions => Convs }

    lazy val becquerelPerSquareMeterSecond =
      Convs.becquerelPerSquareMeterSecond
    lazy val becquerelPerSquareCentimeterSecond =
      Convs.becquerelPerSquareCentimeterSecond

    implicit class ParticleFluxConversions[A](a: A)(
      implicit n: Numeric[A]) extends Convs.ParticleFluxConversions[A](a)

    implicit object ParticleFluxNumeric
        extends AbstractQuantityNumeric[ParticleFluxLike[Tuple], Tuple](
      ParticleFlux)
  }
  val ParticleFlux = ops.particleFluxOps.ParticleFlux

  type Radiance = RadianceLike[Tuple]
  lazy val WattsPerSteradianPerSquareMeter =
    ops.radianceOps.WattsPerSteradianPerSquareMeter

  object RadianceConversions {
    import ops.radianceOps.{ RadianceConversions => Convs }

    lazy val wattPerSteradianPerSquareMeter =
      Convs.wattPerSteradianPerSquareMeter

    implicit class RadianceConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.RadianceConversions[A](a)

    implicit object RadianceNumeric
        extends AbstractQuantityNumeric[RadianceLike[Tuple], Tuple](
      Radiance)
  }
  val Radiance = ops.radianceOps.Radiance

  type RadiantIntensity = RadiantIntensityLike[Tuple]
  lazy val WattsPerSteradian = ops.radiantIntensityOps.WattsPerSteradian

  object RadiantIntensityConversions {
    import ops.radiantIntensityOps.{ RadiantIntensityConversions => Convs }

    lazy val wattPerSteradian = Convs.wattPerSteradian

    implicit class RadiantIntensityConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.RadiantIntensityConversions[A](a)

    implicit object RadiantIntensityNumeric
        extends AbstractQuantityNumeric[RadiantIntensityLike[Tuple], Tuple](
      RadiantIntensity)
  }
  val RadiantIntensity = ops.radiantIntensityOps.RadiantIntensity

  type SpectralIntensity = SpectralIntensityLike[Tuple]
  lazy val WattsPerSteradianPerMeter =
    ops.spectralIntensityOps.WattsPerSteradianPerMeter

  object SpectralIntensityConversions {
    import ops.spectralIntensityOps.{ SpectralIntensityConversions => Convs }

    lazy val wattPerSteradianPerMeter = Convs.wattPerSteradianPerMeter

    implicit class SpectralIntensityConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.SpectralIntensityConversions[A](a)

    implicit object SpectralIntensityNumeric
        extends AbstractQuantityNumeric[SpectralIntensityLike[Tuple], Tuple](
      SpectralIntensity)
  }
  val SpectralIntensity = ops.spectralIntensityOps.SpectralIntensity

  type SpectralIrradiance = SpectralIrradianceLike[Tuple]
  lazy val WattsPerCubicMeterIrradiance =
    ops.spectralIrradianceOps.WattsPerCubicMeterIrradiance
  lazy val WattsPerSquareMeterPerMicron =
    ops.spectralIrradianceOps.WattsPerSquareMeterPerMicron
  lazy val WattsPerSquareMeterPerNanometer =
    ops.spectralIrradianceOps.WattsPerSquareMeterPerNanometer
  lazy val ErgsPerSecondPerSquareCentimeterPerAngstrom =
    ops.spectralIrradianceOps.ErgsPerSecondPerSquareCentimeterPerAngstrom

  object SpectralIrradianceConversions {
    import ops.spectralIrradianceOps.{ SpectralIrradianceConversions => Convs }

    lazy val wattPerCubicMeter = Convs.wattPerCubicMeter
    lazy val wattPerSquareMeterPerNanometer =
      Convs.wattPerSquareMeterPerNanometer
    lazy val wattPerSquareMeterPerMicron = Convs.wattPerSquareMeterPerMicron
    lazy val ergPerSecondPerSquareCentimeterPerAngstrom =
      Convs.ergPerSecondPerSquareCentimeterPerAngstrom

    implicit class SpectralIrradianceConversions[A](a: A)(
      implicit n: Numeric[A])
        extends Convs.SpectralIrradianceConversions[A](a)

    implicit object SpectralIrradianceNumeric
        extends AbstractQuantityNumeric[SpectralIrradianceLike[Tuple], Tuple](
      SpectralIrradiance)
  }
  val SpectralIrradiance = ops.spectralIrradianceOps.SpectralIrradiance

  type SpectralPower = SpectralPowerLike[Tuple]
  lazy val WattsPerMeter = ops.spectralPowerOps.WattsPerMeter

  object SpectralPowerConversions {
    import ops.spectralPowerOps.{ SpectralPowerConversions => Convs }

    lazy val wattPerMeter = Convs.wattPerMeter

    implicit class SpectralPowerConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.SpectralPowerConversions[A](a)

    implicit object SpectralPowerNumeric
        extends AbstractQuantityNumeric[SpectralPowerLike[Tuple], Tuple](
      SpectralPower)
  }
  val SpectralPower = ops.spectralPowerOps.SpectralPower
}
