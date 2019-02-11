/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package information

import scala.reflect.ClassTag

import org.terra.time.{ TimeLike, TimeIntegral }

/**
  * Represents information.
  *
  * @author Derek Morr
  * @since 0.6.0
  * @param value value in [[org.terra.standard.information.Bytes]]
  */
case class InformationLike[C <: TypeContext](
  val value: C#TL, val unit: InformationUnit[C])(implicit ops: TerraOps[C])
    extends Quantity[InformationLike[C], C#TL, C]
    with TimeIntegral[DataRateLike[C], C#TL, C#T, C] {

  override implicit val tttdConverter: HasConverter[C#TT, C#T] =
    ops.converters.tttlConverter.asInstanceOf[HasConverter[C#TT, C#T]]
  //override implicit val ttdConverter: HasConverter[C#T, TD]
  override def makeEnsureType(implicit ops: TerraOps[C]): HasEnsureType[C#TL] =
    ops.converters.ensureTL
  override def makeFromCurrencyConverter(
    implicit ops: TerraOps[C]): HasConverter[C#TC, C#TL] =
    ops.converters.tctlConverter //.asInstanceOf[HasConverter[C#TC, C#TL]]
  override def makeToCurrencyConverter(
    implicit ops: TerraOps[C]): HasConverter[C#TL, C#TC] =
    ops.converters.tltcConverter.asInstanceOf[HasConverter[C#TL, C#TC]]

  override def getNumeric: Numeric[C#TL] = ops.numL
  override def getTag: ClassTag[C#TL] = ops.getClassTagTL

  import ops.informationOps._
  import ops.dataRateOps.BytesPerSecond
  import ops.timeOps.Seconds

  def dimension: Dimension[InformationLike[C], C#TL, C] = Information
  def convLong(l: Long)(implicit ops: TerraOps[C]): C#TL = l.asInstanceOf[C#TL]

  protected def timeDerived = BytesPerSecond(toBytes)
  protected[terra] def time = Seconds(1)

  def toBytes = to(Bytes)
  def toKilobytes = to(Kilobytes)
  def toKibibytes = to(Kibibytes)
  def toMegabytes = to(Megabytes)
  def toMebibytes = to(Mebibytes)
  def toGigabytes = to(Gigabytes)
  def toGibibytes = to(Gibibytes)
  def toTerabytes = to(Terabytes)
  def toTebibytes = to(Tebibytes)
  def toPetabytes = to(Petabytes)
  def toPebibytes = to(Pebibytes)
  def toExabytes = to(Exabytes)
  def toExbibytes = to(Exbibytes)
  def toZettabytes = to(Zettabytes)
  def toZebibytes = to(Zebibytes)
  def toYottabytes = to(Yottabytes)
  def toYobibytes = to(Yobibytes)

  def toBits = to(Bits)
  def toKilobits = to(Kilobits)
  def toKibibits = to(Kibibits)
  def toMegabits = to(Megabits)
  def toMebibits = to(Mebibits)
  def toGigabits = to(Gigabits)
  def toGibibits = to(Gibibits)
  def toTerabits = to(Terabits)
  def toTebibits = to(Tebibits)
  def toPetabits = to(Petabits)
  def toPebibits = to(Pebibits)
  def toExabits = to(Exabits)
  def toExbibits = to(Exbibits)
  def toZettabits = to(Zettabits)
  def toZebibits = to(Zebibits)
  def toYottabits = to(Yottabits)
  def toYobibits = to(Yobibits)
}

trait InformationUnit[C <: TypeContext] 
    extends UnitOfMeasure[InformationLike[C], C#TL, C] with UnitConverter[C#TL, C] {
  def apply(t: C#TL)(
    implicit tag: ClassTag[C#TL], ops: TerraOps[C]): InformationLike[C] =
    new InformationLike[C](t, this)
  override def apply[A](a: A)(
    implicit num: Numeric[A], ops: TerraOps[C]): InformationLike[C] = {
    implicit val tag = getTag
    new InformationLike[C](ops.convLong(num.toLong(a)), this)
  }

  override def makeEnsureType(implicit ops: TerraOps[C]): HasEnsureType[C#TL] =
    ops.converters.ensureTL

  override private[terra] def makeDoubleConverter(
    implicit ops: TerraOps[C]): HasConverter[Double, C#TL] =
    ops.converters.dtlConverter.asInstanceOf[HasConverter[Double, C#TL]]
  override private[terra] def makeLongConverter(
    implicit ops: TerraOps[C]): HasConverter[Long, C#TL] = 
    ops.converters.ltlConverter.asInstanceOf[HasConverter[Long, C#TL]]
    //idConverter.asInstanceOf[HasConverter[Long, C#TL]]
  override def getTag(implicit ops: TerraOps[C]): ClassTag[C#TL] =
    ops.getClassTagTL
}

trait InformationOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]

  def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T
  def convLong(l: Long)(implicit ops: TerraOps[C]): C#TL
  implicit val tagTL: ClassTag[C#TL] = ops.getClassTagTL

  trait InformationUnitT extends InformationUnit[C]

  /**
    * Factory singleton for information
    */
  object Information extends Dimension[InformationLike[C], C#TL, C] 
      with BaseDimension {

    private[information] def apply[A](a: A, unit: InformationUnit[C])(
      implicit n: Numeric[A]) =
      new InformationLike[C](convLong(n.toLong(a)), unit)
    def apply(value: Any) = parseL(value)
    def name = "Information"
    def primaryUnit = Bytes
    def siUnit = Bytes
    def units = Set(Bytes, Kilobytes, Kibibytes, Megabytes, Mebibytes,
      Gigabytes, Gibibytes, Terabytes, Tebibytes, Petabytes, Pebibytes,
      Exabytes, Exbibytes, Zettabytes, Zebibytes, Yottabytes, Yobibytes,
      Bits, Kilobits, Kibibits, Megabits, Mebibits, Gigabits, Gibibits,
      Terabits, Tebibits, Petabits, Pebibits, Exabits, Exbibits,
      Zettabits, Zebibits, Yottabits, Yobibits)
    def dimensionSymbol = "B"
  }

  object Bytes extends InformationUnitT with PrimaryUnit[C#TL, C] with SiBaseUnit {
    val symbol = "B"
  }

  object Octets extends InformationUnitT {
    val conversionFactor = 1.0d
    val symbol = "o"
  }

  object Kilobytes extends InformationUnitT {
    val conversionFactor = MetricSystem.Kilo
    val symbol = "KB"
  }

  object Kibibytes extends InformationUnitT {
    val conversionFactor = BinarySystem.Kilo
    val symbol = "KiB"
  }

  object Megabytes extends InformationUnitT {
    val conversionFactor = MetricSystem.Mega
    val symbol = "MB"
  }

  object Mebibytes extends InformationUnitT {
    val conversionFactor = BinarySystem.Mega
    val symbol = "MiB"
  }

  object Gigabytes extends InformationUnitT {
    val conversionFactor = MetricSystem.Giga
    val symbol = "GB"
  }

  object Gibibytes extends InformationUnitT {
    val conversionFactor = BinarySystem.Giga
    val symbol = "GiB"
  }

  object Terabytes extends InformationUnitT {
    val conversionFactor = MetricSystem.Tera
    val symbol = "TB"
  }

  object Tebibytes extends InformationUnitT {
    val conversionFactor = BinarySystem.Tera
    val symbol = "TiB"
  }

  object Petabytes extends InformationUnitT {
    val conversionFactor = MetricSystem.Peta
    val symbol = "PB"
  }

  object Pebibytes extends InformationUnitT {
    val conversionFactor = BinarySystem.Peta
    val symbol = "PiB"
  }

  object Exabytes extends InformationUnitT {
    val conversionFactor = MetricSystem.Exa
    val symbol = "EB"
  }

  object Exbibytes extends InformationUnitT {
    val conversionFactor = BinarySystem.Exa
    val symbol = "EiB"
  }

  object Zettabytes extends InformationUnitT {
    def conversionFactor = MetricSystem.Zetta
    def symbol = "ZB"
  }

  object Zebibytes extends InformationUnitT {
    def conversionFactor = BinarySystem.Zetta
    def symbol = "ZiB"
  }

  object Yottabytes extends InformationUnitT {
    def conversionFactor = MetricSystem.Yotta
    def symbol = "YB"
  }

  object Yobibytes extends InformationUnitT {
    def conversionFactor = BinarySystem.Yotta
    def symbol = "YiB"
  }

  object Bits extends InformationUnitT {
    def conversionFactor = 0.125d
    val symbol = "bit"
  }

  object Kilobits extends InformationUnitT {
    val conversionFactor = Bits.conversionFactor * MetricSystem.Kilo
    val symbol = "Kbit"
  }

  object Kibibits extends InformationUnitT {
    val conversionFactor = Bits.conversionFactor * BinarySystem.Kilo
    val symbol = "Kibit"
  }

  object Megabits extends InformationUnitT {
    val conversionFactor = Bits.conversionFactor * MetricSystem.Mega
    val symbol = "Mbit"
  }

  object Mebibits extends InformationUnitT {
    val conversionFactor = Bits.conversionFactor * BinarySystem.Mega
    val symbol = "Mibit"
  }

  object Gigabits extends InformationUnitT {
    val conversionFactor = Bits.conversionFactor * MetricSystem.Giga
    val symbol = "Gbit"
  }

  object Gibibits extends InformationUnitT {
    val conversionFactor = Bits.conversionFactor * BinarySystem.Giga
    val symbol = "Gibit"
  }

  object Terabits extends InformationUnitT {
    val conversionFactor = Bits.conversionFactor * MetricSystem.Tera
    val symbol = "Tbit"
  }

  object Tebibits extends InformationUnitT {
    val conversionFactor = Bits.conversionFactor * BinarySystem.Tera
    val symbol = "Tibit"
  }

  object Petabits extends InformationUnitT {
    val conversionFactor = Bits.conversionFactor * MetricSystem.Peta
    val symbol = "Pbit"
  }

  object Pebibits extends InformationUnitT {
    val conversionFactor = Bits.conversionFactor * BinarySystem.Peta
    val symbol = "Pibit"
  }

  object Exabits extends InformationUnitT {
    val conversionFactor = Bits.conversionFactor * MetricSystem.Exa
    val symbol = "Ebit"
  }

  object Exbibits extends InformationUnitT {
    val conversionFactor = Bits.conversionFactor * BinarySystem.Exa
    val symbol = "Eibit"
  }

  object Zettabits extends InformationUnitT {
    def conversionFactor = Bits.conversionFactor * MetricSystem.Zetta
    def symbol = "Zbit"
  }

  object Zebibits extends InformationUnitT {
    def conversionFactor = Bits.conversionFactor * BinarySystem.Zetta
    def symbol = "Zibit"
  }

  object Yottabits extends InformationUnitT {
    def conversionFactor = Bits.conversionFactor * MetricSystem.Yotta
    def symbol = "Ybit"
  }

  object Yobibits extends InformationUnitT {
    def conversionFactor = Bits.conversionFactor * BinarySystem.Yotta
    def symbol = "Yibit"
  }

  object InformationConversions {
    lazy val byte = Bytes(1)
    lazy val kilobyte = Kilobytes(1)
    lazy val kibibyte = Kibibytes(1)
    lazy val megabyte = Megabytes(1)
    lazy val mebibyte = Mebibytes(1)
    lazy val gigabyte = Gigabytes(1)
    lazy val gibibyte = Gibibytes(1)
    lazy val terabyte = Terabytes(1)
    lazy val tebibyte = Tebibytes(1)
    lazy val petabyte = Petabytes(1)
    lazy val pebibyte = Pebibytes(1)
    lazy val exabyte = Exabytes(1)
    lazy val exbibyte = Exbibytes(1)
    lazy val zettabyte = Zettabytes(1)
    lazy val zebibyte = Zebibytes(1)
    lazy val yottabyte = Yottabytes(1)
    lazy val yobibyte = Yobibytes(1)

    lazy val bit = Bits(1)
    lazy val kilobit = Kilobits(1)
    lazy val kibibit = Kibibits(1)
    lazy val megabit = Megabits(1)
    lazy val mebibit = Mebibits(1)
    lazy val gigabit = Gigabits(1)
    lazy val gibibit = Gibibits(1)
    lazy val terabit = Terabits(1)
    lazy val tebibit = Tebibits(1)
    lazy val petabit = Petabits(1)
    lazy val pebibit = Pebibits(1)
    lazy val exabit = Exabits(1)
    lazy val exbibit = Exbibits(1)
    lazy val zettabit = Zettabits(1)
    lazy val zebibit = Zebibits(1)
    lazy val yottabit = Yottabits(1)
    lazy val yobibit = Yobibits(1)

    implicit class InformationConversions[A](a: A)(implicit na: Numeric[A]) {
      def bytes = Bytes(a)
      def kb = Kilobytes(a)
      def kilobytes = Kilobytes(a)
      def mb = Megabytes(a)
      def megabytes = Megabytes(a)
      def gb = Gigabytes(a)
      def gigabytes = Gigabytes(a)
      def tb = Terabytes(a)
      def terabytes = Terabytes(a)
      def pb = Petabytes(a)
      def petabytes = Petabytes(a)
      def eb = Exabytes(a)
      def exabytes = Exabytes(a)
      def zb = Zettabytes(a)
      def zettabytes = Zettabytes(a)
      def yb = Yottabytes(a)
      def yottabytes = Yottabytes(a)

      def kib = Kibibytes(a)
      def kibibytes = Kibibytes(a)
      def mib = Mebibytes(a)
      def mebibytes = Mebibytes(a)
      def gib = Gibibytes(a)
      def gibibytes = Gibibytes(a)
      def tib = Tebibytes(a)
      def tebibytes = Tebibytes(a)
      def pib = Pebibytes(a)
      def pebibytes = Pebibytes(a)
      def eib = Exbibytes(a)
      def exbibytes = Exbibytes(a)
      def zib = Zebibytes(a)
      def zebibytes = Zebibytes(a)
      def yib = Yobibytes(a)
      def yobibytes = Yobibytes(a)

      def bits = Bits(a)
      def kilobits = Kilobits(a)
      def megabits = Megabits(a)
      def gigabits = Gigabits(a)
      def terabits = Terabits(a)
      def petabits = Petabits(a)
      def exabits = Exabits(a)
      def zettabits = Zettabits(a)
      def yottabits = Yottabits(a)

      def kibibits = Kibibits(a)
      def mebibits = Mebibits(a)
      def gibibits = Gibibits(a)
      def tebibits = Tebibits(a)
      def pebibits = Pebibits(a)
      def exbibits = Exbibits(a)
      def zebibits = Zebibits(a)
      def yobibits = Yobibits(a)
    }

    implicit class InformationStringConversions(s: String) {
      def toInformation = Information(s)
    }
  }
}

