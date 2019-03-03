
package org.terra
package mass

import time.TimeDerivative

/**
  * @author Hunter Payne
  */
final class CatalyticActivityLike[C <: TypeContext](
  val value: C#T, val unit: CatalyticActivityUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[CatalyticActivityLike[C], C#T, C]
    with TimeDerivative[ChemicalAmountLike[C], C#T, C#T, C] {

  import ops.catalyticActivityOps._
  import ops.chemicalAmountOps.Moles
  import ops.timeOps.Seconds

  def dimension: Dimension[CatalyticActivityLike[C], C#T, C] = CatalyticActivity

  protected[terra] def timeIntegrated = Moles(toKatals)
  protected[terra] def time = Seconds(1)

  def toKatals = to(Katals) // 1 mol / second
  def toDecikatals = to(Decikatals)
  def toCentikatals = to(Centikatals)
  def toMillikatals = to(Millikatals)
  def toMicrokatals = to(Microkatals)
  def toNanokatals = to(Nanokatals)
  def toDecakatals = to(Decakatals)
  def toHectokatals = to(Hectokatals)
  def toKilokatals = to(Kilokatals)
  def toMegakatals = to(Megakatals)
  def toGigakatals = to(Gigakatals)
}

trait CatalyticActivityUnit[C <: TypeContext] 
    extends UnitOfMeasure[CatalyticActivityLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) = 
    new CatalyticActivityLike[C](t, this)
}

trait CatalyticActivityOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]
  //def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait CatalyticActivityUnitT extends CatalyticActivityUnit[C]

  object CatalyticActivity extends Dimension[CatalyticActivityLike[C], C#T, C] 
      with BaseDimension {
    private[mass] def apply[A](a: A, unit: CatalyticActivityUnit[C])(
      implicit n: Numeric[A]) = 
      new CatalyticActivityLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    val name = "CatalyticActivity"
    def primaryUnit = Katals
    def siUnit = Katals
    def units = Set(
      Katals, Decikatals, Centikatals, Millikatals, Microkatals, Nanokatals, 
      Decakatals, Hectokatals, Kilokatals, Megakatals, Gigakatals)
    def dimensionSymbol = "z"
  }

  object Katals extends CatalyticActivityUnitT with PrimaryUnit[C#T, C] 
      with SiBaseUnit {
    val symbol = "kat"
  }

  object Decikatals extends CatalyticActivityUnitT {
    val symbol = "dkat"
    val conversionFactor = MetricSystem.Deci
  }

  object Centikatals extends CatalyticActivityUnitT {
    val symbol = "ckat"
    val conversionFactor = MetricSystem.Centi
  }

  object Millikatals extends CatalyticActivityUnitT {
    val symbol = "mkat"
    val conversionFactor = MetricSystem.Milli
  }

  object Microkatals extends CatalyticActivityUnitT {
    val symbol = "µkat"
    val conversionFactor = MetricSystem.Micro
  }

  object Nanokatals extends CatalyticActivityUnitT {
    val symbol = "nkat"
    val conversionFactor = MetricSystem.Nano
  }

  object Decakatals extends CatalyticActivityUnitT {
    val symbol = "dakat"
    val conversionFactor = MetricSystem.Deca
  }

  object Hectokatals extends CatalyticActivityUnitT {
    val symbol = "hkat"
    val conversionFactor = MetricSystem.Hecto
  }

  object Kilokatals extends CatalyticActivityUnitT {
    val symbol = "kkat"
    val conversionFactor = MetricSystem.Kilo
  }

  object Megakatals extends CatalyticActivityUnitT {
    val symbol = "Mkat"
    val conversionFactor = MetricSystem.Mega
  }

  object Gigakatals extends CatalyticActivityUnitT {
    val symbol = "Gkat"
    val conversionFactor = MetricSystem.Giga
  }

  object CatalyticActivityConversions {
    lazy val katal = Katals(1)
    lazy val decikatal = Decikatals(1)
    lazy val centikatal = Centikatals(1)
    lazy val millikatal = Millikatals(1)
    lazy val microkatal = Microkatals(1)
    lazy val nanokatal = Nanokatals(1)
    lazy val decakatal = Decakatals(1)
    lazy val hectokatal = Hectokatals(1)
    lazy val kilokatal = Kilokatals(1)
    lazy val megakatal = Megakatals(1)
    lazy val gigakatal = Gigakatals(1)

    implicit class CatalyticActivityConversions[A](a: A)(implicit n: Numeric[A]) {
      def katals = Katals(a)
      def decikatals = Decikatals(a)
      def centikatals = Centikatals(a)
      def millikatals = Millikatals(a)
      def microkatals = Microkatals(a)
      def nanokatals = Nanokatals(a)
      def decakatals = Decakatals(a)
      def hectokatals = Hectokatals(a)
      def kilokatals = Kilokatals(a)
      def megakatals = Megakatals(a)
      def gigakatals = Gigakatals(a)
    }
  }
}
