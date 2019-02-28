
package org.terra
package motion

/**
  * Contains all the symbols exported by the org.terra.*.motion packages.
  * Synthetic packages then subclass this trait with an object to represent
  * a package in the synthetic trees of symbols used by the unit test and
  * user's calling code.
  * @author Hunter Payne
  */
trait MotionSymbols[Tuple <: TypeContext] {

  implicit val ops: TerraOps[Tuple]

  type Acceleration = AccelerationLike[Tuple]
  lazy val FeetPerSecondSquared = ops.accelerationOps.FeetPerSecondSquared
  lazy val MillimetersPerSecondSquared =
    ops.accelerationOps.MillimetersPerSecondSquared
  lazy val MetersPerSecondSquared = ops.accelerationOps.MetersPerSecondSquared
  lazy val UsMilesPerHourSquared = ops.accelerationOps.UsMilesPerHourSquared
  lazy val EarthGravities = ops.accelerationOps.EarthGravities

  object AccelerationConversions {
    import ops.accelerationOps.{ AccelerationConversions => Convs }

    implicit class AccelerationConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.AccelerationConversions[A](a)

    implicit object AccelerationNumeric
        extends AbstractQuantityNumeric[AccelerationLike[Tuple], Tuple](
      Acceleration)
  }
  val Acceleration = ops.accelerationOps.Acceleration

  type AngularAcceleration = AngularAccelerationLike[Tuple]
  lazy val RadiansPerSecondSquared =
    ops.angularAccelerationOps.RadiansPerSecondSquared
  lazy val DegreesPerSecondSquared =
    ops.angularAccelerationOps.DegreesPerSecondSquared
  lazy val GradiansPerSecondSquared =
    ops.angularAccelerationOps.GradiansPerSecondSquared
  lazy val TurnsPerSecondSquared =
    ops.angularAccelerationOps.TurnsPerSecondSquared
  lazy val ArcminutesPerSecondSquared =
    ops.angularAccelerationOps.ArcminutesPerSecondSquared
  lazy val ArcsecondsPerSecondSquared =
    ops.angularAccelerationOps.ArcsecondsPerSecondSquared
  object AngularAccelerationConversions {
    import ops.angularAccelerationOps.{ AngularAccelerationConversions => Convs }
    lazy val radianPerSecondSquared = Convs.radianPerSecondSquared
    lazy val degreePerSecondSquared = Convs.degreePerSecondSquared
    lazy val gradPerSecondSquared = Convs.gradPerSecondSquared
    lazy val turnPerSecondSquared = Convs.turnPerSecondSquared

    implicit class AngularAccelerationConversions[A](a: A)(
      implicit n: Numeric[A])
        extends Convs.AngularAccelerationConversions[A](a)

    implicit object AngularAccelerationNumeric
        extends AbstractQuantityNumeric[AngularAccelerationLike[Tuple], Tuple](
      AngularAcceleration)
  }
  val AngularAcceleration = ops.angularAccelerationOps.AngularAcceleration

  type AngularVelocity = AngularVelocityLike[Tuple]
  lazy val RadiansPerSecond = ops.angularVelocityOps.RadiansPerSecond
  lazy val DegreesPerSecond = ops.angularVelocityOps.DegreesPerSecond
  lazy val GradiansPerSecond = ops.angularVelocityOps.GradiansPerSecond
  lazy val TurnsPerSecond = ops.angularVelocityOps.TurnsPerSecond

  object AngularVelocityConversions {
    import ops.angularVelocityOps.{ AngularVelocityConversions => Convs }

    lazy val radianPerSecond = Convs.radianPerSecond
    lazy val degreePerSecond = Convs.degreePerSecond
    lazy val gradPerSecond = Convs.gradPerSecond
    lazy val gradianPerSecond = Convs.gradianPerSecond
    lazy val turnPerSecond = Convs.turnPerSecond

    implicit class AngularVelocityConversions[A](val a: A)(
      implicit n: Numeric[A])
        extends Convs.AngularVelocityConversions[A](a)

    implicit object AngularVelocityNumeric
        extends AbstractQuantityNumeric[AngularVelocityLike[Tuple], Tuple](
      AngularVelocity)
  }
  val AngularVelocity = ops.angularVelocityOps.AngularVelocity

  type Force = ForceLike[Tuple]
  lazy val Newtons = ops.forceOps.Newtons
  lazy val KilogramForce = ops.forceOps.KilogramForce
  lazy val PoundForce = ops.forceOps.PoundForce
  lazy val KiloElectronVoltsPerMicrometer =
    ops.forceOps.KiloElectronVoltsPerMicrometer
  lazy val MegaElectronVoltsPerCentimeter =
    ops.forceOps.MegaElectronVoltsPerCentimeter

  object ForceConversions {
    import ops.forceOps.{ ForceConversions => Convs }

    lazy val newton = Convs.newton
    lazy val kilogramForce = Convs.kilogramForce
    lazy val poundForce = Convs.poundForce
    lazy val kiloElectronVoltPerMicrometer =
      Convs.kiloElectronVoltPerMicrometer
    lazy val megaElectronVoltPerCentimeter =
      Convs.megaElectronVoltPerCentimeter

    implicit class ForceConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.ForceConversions[A](a)

    implicit object ForceNumeric
        extends AbstractQuantityNumeric[ForceLike[Tuple], Tuple](Force)
  }
  val Force = ops.forceOps.Force

  type Jerk = JerkLike[Tuple]
  lazy val MetersPerSecondCubed = ops.jerkOps.MetersPerSecondCubed
  lazy val FeetPerSecondCubed = ops.jerkOps.FeetPerSecondCubed

  object JerkConversions {
    import ops.jerkOps.{ JerkConversions => Convs }

    lazy val meterPerSecondCubed = Convs.meterPerSecondCubed
    lazy val footPerSecondCubed = Convs.footPerSecondCubed

    implicit class JerkConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.JerkConversions[A](a)

    implicit object JerkNumeric
        extends AbstractQuantityNumeric[JerkLike[Tuple], Tuple](Jerk)
  }
  val Jerk = ops.jerkOps.Jerk

  type MassFlow = MassFlowLike[Tuple]
  lazy val KilogramsPerSecond = ops.massFlowOps.KilogramsPerSecond
  lazy val PoundsPerSecond = ops.massFlowOps.PoundsPerSecond
  lazy val PoundsPerHour = ops.massFlowOps.PoundsPerHour
  lazy val KilopoundsPerHour = ops.massFlowOps.KilopoundsPerHour
  lazy val MegapoundsPerHour = ops.massFlowOps.MegapoundsPerHour

  object MassFlowConversions {
    import ops.massFlowOps.{ MassFlowConversions => Convs }

    lazy val kilogramPerSecond = Convs.kilogramPerSecond
    lazy val poundPerSecond = Convs.poundPerSecond
    lazy val poundPerHour = Convs.poundPerHour
    lazy val kilopoundPerHour = Convs.kilopoundPerHour
    lazy val megapoundPerHour = Convs.megapoundPerHour

    implicit class MassFlowConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.MassFlowConversions[A](a)

    implicit object MassFlowNumeric
        extends AbstractQuantityNumeric[MassFlowLike[Tuple], Tuple](MassFlow)
  }
  val MassFlow = ops.massFlowOps.MassFlow

  type Momentum = MomentumLike[Tuple]
  lazy val NewtonSeconds = ops.momentumOps.NewtonSeconds

  object MomentumConversions {
    import ops.momentumOps.{ MomentumConversions => Convs }

    lazy val newtonSecond = Convs.newtonSecond

    implicit class MomentumConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.MomentumConversions[A](a)

    implicit object MomentumNumeric
        extends AbstractQuantityNumeric[MomentumLike[Tuple], Tuple](Momentum)
  }
  val Momentum = ops.momentumOps.Momentum

  type Pressure = PressureLike[Tuple]
  lazy val Pascals = ops.pressureOps.Pascals
  lazy val Bars = ops.pressureOps.Bars
  lazy val PoundsPerSquareInch = ops.pressureOps.PoundsPerSquareInch
  lazy val StandardAtmospheres = ops.pressureOps.StandardAtmospheres
  lazy val MillimetersOfMercury = ops.pressureOps.MillimetersOfMercury
  lazy val InchesOfMercury = ops.pressureOps.InchesOfMercury
  lazy val Torrs = ops.pressureOps.Torrs

  object PressureConversions {
    import ops.pressureOps.{ PressureConversions => Convs }

    lazy val pascal = Convs.pascal
    lazy val bar = Convs.bar
    lazy val psi = Convs.psi
    lazy val atm = Convs.atm
    lazy val mmHg = Convs.mmHg
    lazy val inHg = Convs.inHg
    lazy val torr = Convs.torr

    implicit class PressureConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.PressureConversions[A](a)

    implicit object PressureNumeric
        extends AbstractQuantityNumeric[PressureLike[Tuple], Tuple](Pressure)
  }
  val Pressure = ops.pressureOps.Pressure

  type PressureChange = PressureChangeLike[Tuple]
  lazy val PascalsPerSecond = ops.pressureChangeOps.PascalsPerSecond
  lazy val BarsPerSecond = ops.pressureChangeOps.BarsPerSecond
  lazy val PoundsPerSquareInchPerSecond =
    ops.pressureChangeOps.PoundsPerSquareInchPerSecond
  lazy val StandardAtmospheresPerSecond =
    ops.pressureChangeOps.StandardAtmospheresPerSecond

  object PressureChangeConversions {
    import ops.pressureChangeOps.{ PressureChangeConversions => Convs }

    lazy val pascalPerSecond = Convs.pascalPerSecond
    lazy val barPerSecond = Convs.barPerSecond
    lazy val poundPerSquareInchPerSecond = Convs.poundPerSquareInchPerSecond
    lazy val standardAtmospherePerSecond = Convs.standardAtmospherePerSecond

    implicit class PressureChangeConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.PressureChangeConversions[A](a)

    implicit object PressureChangeNumeric
        extends AbstractQuantityNumeric[PressureChangeLike[Tuple], Tuple](
      PressureChange)
  }
  val PressureChange = ops.pressureChangeOps.PressureChange

  type Torque = TorqueLike[Tuple]
  lazy val NewtonMeters = ops.torqueOps.NewtonMeters
  lazy val PoundFeet = ops.torqueOps.PoundFeet

  object TorqueConversions {
    import ops.torqueOps.{ TorqueConversions => Convs }

    lazy val newtonMeter = Convs.newtonMeter
    lazy val poundFoot = Convs.poundFoot

    implicit class TorqueConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.TorqueConversions[A](a)

    implicit object TorqueNumeric
        extends AbstractQuantityNumeric[TorqueLike[Tuple], Tuple](Torque)
  }
  val Torque = ops.torqueOps.Torque

  type Velocity = VelocityLike[Tuple]
  lazy val MetersPerSecond = ops.velocityOps.MetersPerSecond
  lazy val FeetPerSecond = ops.velocityOps.FeetPerSecond
  lazy val MillimetersPerSecond = ops.velocityOps.MillimetersPerSecond
  lazy val KilometersPerSecond = ops.velocityOps.KilometersPerSecond
  lazy val KilometersPerHour = ops.velocityOps.KilometersPerHour
  lazy val UsMilesPerHour = ops.velocityOps.UsMilesPerHour
  lazy val InternationalMilesPerHour =
    ops.velocityOps.InternationalMilesPerHour
  lazy val Knots = ops.velocityOps.Knots

  object VelocityConversions {
    import ops.velocityOps.{ VelocityConversions => Convs }

    lazy val footPerSecond = Convs.footPerSecond
    lazy val millimeterPerSecond = Convs.millimeterPerSecond
    lazy val meterPerSecond = Convs.meterPerSecond
    lazy val kilometerPerSecond = Convs.kilometerPerSecond
    lazy val kilometerPerHour = Convs.kilometerPerHour
    lazy val milePerHour = Convs.milePerHour
    lazy val knot = Convs.knot

    implicit class VelocityConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.VelocityConversions[A](a)

    implicit object VelocityNumeric
        extends AbstractQuantityNumeric[VelocityLike[Tuple], Tuple](Velocity)
  }
  val Velocity = ops.velocityOps.Velocity

  type VolumeFlow = VolumeFlowLike[Tuple]
  lazy val CubicMetersPerSecond = ops.volumeFlowOps.CubicMetersPerSecond
  lazy val CubicFeetPerHour = ops.volumeFlowOps.CubicFeetPerHour
  lazy val GallonsPerDay = ops.volumeFlowOps.GallonsPerDay
  lazy val GallonsPerHour = ops.volumeFlowOps.GallonsPerHour
  lazy val GallonsPerMinute = ops.volumeFlowOps.GallonsPerMinute
  lazy val GallonsPerSecond = ops.volumeFlowOps.GallonsPerSecond

  object VolumeFlowConversions {
    import ops.volumeFlowOps.{ VolumeFlowConversions => Convs }

    lazy val cubicMeterPerSecond = Convs.cubicMeterPerSecond
    lazy val cubicFootPerHour = Convs.cubicFootPerHour
    lazy val gallonPerDay = Convs.gallonPerDay
    lazy val gallonPerHour = Convs.gallonPerHour
    lazy val gallonPerMinute = Convs.gallonPerMinute
    lazy val gallonPerSecond = Convs.gallonPerSecond

    implicit class VolumeFlowConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.VolumeFlowConversions[A](a)

    implicit object VolumeFlowNumeric
        extends AbstractQuantityNumeric[VolumeFlowLike[Tuple], Tuple](
      VolumeFlow)
  }
  val VolumeFlow = ops.volumeFlowOps.VolumeFlow

  type Yank = YankLike[Tuple]
  lazy val NewtonsPerSecond = ops.yankOps.NewtonsPerSecond

  object YankConversions {
    import ops.yankOps.{ YankConversions => Convs }

    lazy val newtonPerSecond = Convs.newtonPerSecond

    implicit class YankConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.YankConversions[A](a)

    implicit object YankNumeric
        extends AbstractQuantityNumeric[YankLike[Tuple], Tuple](Yank)
  }
  val Yank = ops.yankOps.Yank

  type SurfaceTension = SurfaceTensionLike[Tuple]
  lazy val NewtonsPerMeter = ops.surfaceTensionOps.NewtonsPerMeter
  lazy val PoundsPerFoot = ops.surfaceTensionOps.PoundsPerFoot

  object SurfaceTensionConversions {
    import ops.surfaceTensionOps.{ SurfaceTensionConversions => Convs }

    lazy val newtonPerMeter = Convs.newtonPerMeter
    lazy val poundPerFoot = Convs.poundPerFoot

    implicit class SurfaceTensionConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.SurfaceTensionConversions[A](a)

    implicit object SurfaceTensionNumeric
        extends AbstractQuantityNumeric[SurfaceTensionLike[Tuple], Tuple](
      SurfaceTension)
  }
  val SurfaceTension = ops.surfaceTensionOps.SurfaceTension

  type Viscosity = ViscosityLike[Tuple]
  lazy val PascalSeconds = ops.viscosityOps.PascalSeconds
  lazy val KilogramsPerMeterSecond = ops.viscosityOps.KilogramsPerMeterSecond
  lazy val Poise = ops.viscosityOps.Poise
  lazy val CentiPoise = ops.viscosityOps.CentiPoise

  object ViscosityConversions {
    import ops.viscosityOps.{ ViscosityConversions => Convs }

    lazy val pascalSecond = Convs.pascalSecond
    lazy val kilogramPerMeterSecond = Convs.kilogramPerMeterSecond
    lazy val poise = Convs.poise
    lazy val centipoise = Convs.centipoise

    implicit class ViscosityConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.ViscosityConversions[A](a)

    implicit object ViscosityNumeric
        extends AbstractQuantityNumeric[ViscosityLike[Tuple], Tuple](
      Viscosity)
  }
  val Viscosity = ops.viscosityOps.Viscosity

  lazy val SpeedOfLight = MetersPerSecond(2.99792458e8)
  lazy val EquatorGravity = MetersPerSecondSquared(9.7903)
  lazy val StandardEarthGravity = MetersPerSecondSquared(9.80665)
  lazy val PoleGravity = MetersPerSecondSquared(9.8322)
}
