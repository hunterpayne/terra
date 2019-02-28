
package org.terra
package motion

import scala.reflect.ClassTag

import org.terra.space.LengthLike
import org.terra.time.{ TimeDerivative, TimeIntegral, TimeLike }

/**
  *
  */
final class ViscosityLike[C <: TypeContext](
  val value: C#T, val unit: ViscosityUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[ViscosityLike[C], C#T, C] 
    with TimeIntegral[PressureLike[C], C#T, C#T, C] {

  import ops.viscosityOps._
  import ops.pressureOps.Pascals
  import ops.timeOps.Seconds

  type Pressure = PressureLike[C]

  def dimension: Dimension[ViscosityLike[C], C#T, C] = Viscosity

  override protected def timeDerived: Pressure = {
    implicit val opsArg = ops
    Pascals(toPascalSeconds)
  }
  override protected def time: Time = {
    implicit val opsArg = ops
    Seconds(1)
  }

  // Pa·s 
  def toPascalSeconds = to(PascalSeconds)
  // or kg·m−1·s−1
  def toKilogramsPerMeterSecond = to(KilogramsPerMeterSecond)
  def toPoise = to(Poise)
  def toCentiPoise = to(CentiPoise)
}

trait ViscosityUnit[C <: TypeContext] 
    extends UnitOfMeasure[ViscosityLike[C], C#T, C]
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) =
    new ViscosityLike[C](t, this)
}

trait ViscosityOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]
  def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait ViscosityUnitT extends ViscosityUnit[C]

  object Viscosity extends Dimension[ViscosityLike[C], C#T, C] {
    private[motion] def apply[A](a: A, unit: ViscosityUnit[C])(
      implicit n: Numeric[A]) =
      new ViscosityLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "Viscosity"
    def primaryUnit = PascalSeconds
    def siUnit = PascalSeconds
    def units = Set(PascalSeconds, KilogramsPerMeterSecond, Poise, CentiPoise)
  }

  import ops.pressureOps.Pascals
  import ops.massOps.Kilograms
  import ops.lengthOps.Meters
  import ops.timeOps.Seconds

  object PascalSeconds extends ViscosityUnitT with PrimaryUnit[C#T, C]
      with SiUnit {
    val symbol = Pascals.symbol + "·" + Seconds.symbol
  }

  object KilogramsPerMeterSecond extends ViscosityUnitT 
      with UnitConverter[C#T, C] {
    val symbol = Kilograms.symbol + "/" + Meters.symbol + "·" + Seconds.symbol
    val conversionFactor = 1.0
  }

  object Poise extends ViscosityUnitT with UnitConverter[C#T, C] {
    val symbol = "P"
    val conversionFactor = 0.1
  }

  object CentiPoise extends ViscosityUnitT with UnitConverter[C#T, C] {
    val symbol = "cP"
    val conversionFactor = 0.001
  }

  object ViscosityConversions {
    lazy val pascalSecond = PascalSeconds(1)
    lazy val kilogramPerMeterSecond = KilogramsPerMeterSecond(1)
    lazy val poise = Poise(1)
    lazy val centipoise = CentiPoise(1)

    implicit class ViscosityConversions[A](a: A)(implicit n: Numeric[A]) {
      def pascalSeconds = PascalSeconds(a)
      def kilogramsPerMeterSecond = KilogramsPerMeterSecond(a)
      def poise = Poise(a)
      def centipoise = CentiPoise(a)
    }
  }
}
