
package org.terra
package information

import scala.util.Try
import scala.reflect.ClassTag

import org.terra.time._

/**
  * Represents a rate of transfer of information
  */
final class DataRateLike[C <: TypeContext](
  val value: C#T, val unit: DataRateUnit[C])(implicit ops: TerraOps[C])
    extends Quantity[DataRateLike[C], C#T, C]
    with TimeDerivative[InformationLike[C], C#T, C#TL, C] {

  import ops.dataRateOps._
  import ops.informationOps.Bytes
  import ops.timeOps.Seconds

  override implicit val tttiConverter: HasConverter[C#TT, C#TL] =
    ops.converters.tttlConverter

  override implicit val num: Numeric[C#T] = ops.num

  def dimension: Dimension[DataRateLike[C], C#T, C] = DataRate

  protected[terra] def timeIntegrated = Bytes(toBytesPerSecond)
  protected[terra] def time = Seconds(1)

  def toBytesPerSecond = to(BytesPerSecond)
  def toKilobytesPerSecond = to(KilobytesPerSecond)
  def toMegabytesPerSecond = to(MegabytesPerSecond)
  def toGigabytesPerSecond = to(GigabytesPerSecond)
  def toTerabytesPerSecond = to(TerabytesPerSecond)
  def toPetabytesPerSecond = to(PetabytesPerSecond)
  def toExabytesPerSecond = to(ExabytesPerSecond)
  def toZettabytesPerSecond = to(ZettabytesPerSecond)
  def toYottabytesPerSecond = to(YottabytesPerSecond)

  def toKibibytesPerSecond = to(KibibytesPerSecond)
  def toMebibytesPerSecond = to(MebibytesPerSecond)
  def toGibibytesPerSecond = to(GibibytesPerSecond)
  def toTebibytesPerSecond = to(TebibytesPerSecond)
  def toPebibytesPerSecond = to(PebibytesPerSecond)
  def toExbibytesPerSecond = to(ExbibytesPerSecond)
  def toZebibytesPerSecond = to(ZebibytesPerSecond)
  def toYobibytesPerSecond = to(YobibytesPerSecond)

  def toBitsPerSecond = to(BitsPerSecond)
  def toKilobitsPerSecond = to(KilobitsPerSecond)
  def toMegabitsPerSecond = to(MegabitsPerSecond)
  def toGigabitsPerSecond = to(GigabitsPerSecond)
  def toTerabitsPerSecond = to(TerabitsPerSecond)
  def toPetabitsPerSecond = to(PetabitsPerSecond)
  def toExabitsPerSecond = to(ExabitsPerSecond)
  def toZettabitsPerSecond = to(ZettabitsPerSecond)
  def toYottabitsPerSecond = to(YottabitsPerSecond)

  def toKibibitsPerSecond = to(KibibitsPerSecond)
  def toMebibitsPerSecond = to(MebibitsPerSecond)
  def toGibibitsPerSecond = to(GibibitsPerSecond)
  def toTebibitsPerSecond = to(TebibitsPerSecond)
  def toPebibitsPerSecond = to(PebibitsPerSecond)
  def toExbibitsPerSecond = to(ExbibitsPerSecond)
  def toZebibitsPerSecond = to(ZebibitsPerSecond)
  def toYobibitsPerSecond = to(YobibitsPerSecond)
}

trait DataRateUnit[C <: TypeContext] extends UnitOfMeasure[DataRateLike[C], C#T, C] with UnitConverter[C#T, C] {
  //def apply[A](a: A)(implicit num: Numeric[A], ops: TerraOps[C]) =
    //new DataRateLike[C](ops.convDouble(num.toDouble(a)), this)
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) = 
    new DataRateLike[C](t, this)
}

trait DataRateOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]
  def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  import ops.dataRateOps.BytesPerSecond

  trait DataRateUnitT extends DataRateUnit[C]

  object DataRate extends Dimension[DataRateLike[C], C#T, C] {
    private[information] def apply[A](a: A, unit: DataRateUnit[C])(
      implicit num: Numeric[A]): DataRateLike[C] =
      new DataRateLike[C](convDouble(num.toDouble(a)), unit)

    def apply(i: InformationLike[C], t: TimeLike[C]) = {
      implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
      implicit val tag: ClassTag[C#T] = ops.getClassTagT
      BytesPerSecond(ops.div[C#T](
        ops.rconv(i.toBytes), ops.rconvT(t.toSeconds)))
    }
    def apply(value: Any)(implicit ops: TerraOps[C]): Try[DataRateLike[C]] =
      parse(value)
    def name = "DataRate"
    def primaryUnit = BytesPerSecond
    def siUnit = BytesPerSecond
    def units = Set(BytesPerSecond, KilobytesPerSecond, KibibytesPerSecond, MegabytesPerSecond, MebibytesPerSecond,
      GigabytesPerSecond, GibibytesPerSecond, TerabytesPerSecond, TebibytesPerSecond,
      PetabytesPerSecond, PebibytesPerSecond, ExabytesPerSecond, ExbibytesPerSecond,
      ZettabytesPerSecond, ZebibytesPerSecond, YottabytesPerSecond, YobibytesPerSecond,
      BitsPerSecond, KilobitsPerSecond, KibibitsPerSecond, MegabitsPerSecond, MebibitsPerSecond,
      GigabitsPerSecond, GibibitsPerSecond, TerabitsPerSecond, TebibitsPerSecond,
      PetabitsPerSecond, PebibitsPerSecond, ExabitsPerSecond, ExbibitsPerSecond,
      ZettabitsPerSecond, ZebibitsPerSecond, YottabitsPerSecond, YobibitsPerSecond)
  }

  object BytesPerSecond extends DataRateUnitT with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = "B/s"
  }

  object KilobytesPerSecond extends DataRateUnitT {
    val symbol = "KB/s"
    val conversionFactor = ops.informationOps.Kilobytes.conversionFactor
  }

  object KibibytesPerSecond extends DataRateUnitT {
    val symbol = "KiB/s"
    val conversionFactor = ops.informationOps.Kibibytes.conversionFactor
  }

  object MegabytesPerSecond extends DataRateUnitT {
    val symbol = "MB/s"
    val conversionFactor = ops.informationOps.Megabytes.conversionFactor
  }

  object MebibytesPerSecond extends DataRateUnitT {
    val symbol = "MiB/s"
    val conversionFactor = ops.informationOps.Mebibytes.conversionFactor
  }

  object GigabytesPerSecond extends DataRateUnitT {
    val symbol = "GB/s"
    val conversionFactor = ops.informationOps.Gigabytes.conversionFactor
  }

  object GibibytesPerSecond extends DataRateUnitT {
    val symbol = "GiB/s"
    val conversionFactor = ops.informationOps.Gibibytes.conversionFactor
  }

  object TerabytesPerSecond extends DataRateUnitT {
    val symbol = "TB/s"
    val conversionFactor = ops.informationOps.Terabytes.conversionFactor
  }

  object TebibytesPerSecond extends DataRateUnitT {
    val symbol = "TiB/s"
    val conversionFactor = ops.informationOps.Tebibytes.conversionFactor
  }

  object PetabytesPerSecond extends DataRateUnitT {
    val symbol = "PB/s"
    val conversionFactor = ops.informationOps.Petabytes.conversionFactor
  }

  object PebibytesPerSecond extends DataRateUnitT {
    val symbol = "PiB/s"
    val conversionFactor = ops.informationOps.Pebibytes.conversionFactor
  }

  object ExabytesPerSecond extends DataRateUnitT {
    val symbol = "EB/s"
    val conversionFactor = ops.informationOps.Exabytes.conversionFactor
  }

  object ExbibytesPerSecond extends DataRateUnitT {
    val symbol = "EiB/s"
    val conversionFactor = ops.informationOps.Exbibytes.conversionFactor
  }

  object ZettabytesPerSecond extends DataRateUnitT {
    val symbol = "ZB/s"
    val conversionFactor = ops.informationOps.Zettabytes.conversionFactor
  }

  object ZebibytesPerSecond extends DataRateUnitT {
    val symbol = "ZiB/s"
    val conversionFactor = ops.informationOps.Zebibytes.conversionFactor
  }

  object YottabytesPerSecond extends DataRateUnitT {
    val symbol = "YB/s"
    val conversionFactor = ops.informationOps.Yottabytes.conversionFactor
  }

  object YobibytesPerSecond extends DataRateUnitT {
    val symbol = "YiB/s"
    val conversionFactor = ops.informationOps.Yobibytes.conversionFactor
  }

  object BitsPerSecond extends DataRateUnitT {
    val symbol = "bps"
    val conversionFactor = ops.informationOps.Bits.conversionFactor
  }

  object KilobitsPerSecond extends DataRateUnitT {
    val symbol = "Kbps"
    val conversionFactor = BitsPerSecond.conversionFactor * ops.informationOps.Kilobytes.conversionFactor
  }

  object KibibitsPerSecond extends DataRateUnitT {
    val symbol = "Kibps"
    val conversionFactor = BitsPerSecond.conversionFactor * ops.informationOps.Kibibytes.conversionFactor
  }

  object MegabitsPerSecond extends DataRateUnitT {
    val symbol = "Mbps"
    val conversionFactor = BitsPerSecond.conversionFactor * ops.informationOps.Megabytes.conversionFactor
  }

  object MebibitsPerSecond extends DataRateUnitT {
    val symbol = "Mibps"
    val conversionFactor = BitsPerSecond.conversionFactor * ops.informationOps.Mebibytes.conversionFactor
  }

  object GigabitsPerSecond extends DataRateUnitT {
    val symbol = "Gbps"
    val conversionFactor = BitsPerSecond.conversionFactor * ops.informationOps.Gigabytes.conversionFactor
  }

  object GibibitsPerSecond extends DataRateUnitT {
    val symbol = "Gibps"
    val conversionFactor = BitsPerSecond.conversionFactor * ops.informationOps.Gibibytes.conversionFactor
  }

  object TerabitsPerSecond extends DataRateUnitT {
    val symbol = "Tbps"
    val conversionFactor = BitsPerSecond.conversionFactor * ops.informationOps.Terabytes.conversionFactor
  }

  object TebibitsPerSecond extends DataRateUnitT {
    val symbol = "Tibps"
    val conversionFactor = BitsPerSecond.conversionFactor * ops.informationOps.Tebibytes.conversionFactor
  }

  object PetabitsPerSecond extends DataRateUnitT {
    val symbol = "Pbps"
    val conversionFactor = BitsPerSecond.conversionFactor * ops.informationOps.Petabytes.conversionFactor
  }

  object PebibitsPerSecond extends DataRateUnitT {
    val symbol = "Pibps"
    val conversionFactor = BitsPerSecond.conversionFactor * ops.informationOps.Pebibytes.conversionFactor
  }

  object ExabitsPerSecond extends DataRateUnitT {
    val symbol = "Ebps"
    val conversionFactor = BitsPerSecond.conversionFactor * ops.informationOps.Exabytes.conversionFactor
  }

  object ExbibitsPerSecond extends DataRateUnitT {
    val symbol = "Eibps"
    val conversionFactor = BitsPerSecond.conversionFactor * ops.informationOps.Exbibytes.conversionFactor
  }

  object ZettabitsPerSecond extends DataRateUnitT {
    val symbol = "Zbps"
    val conversionFactor = BitsPerSecond.conversionFactor * ops.informationOps.Zettabytes.conversionFactor
  }

  object ZebibitsPerSecond extends DataRateUnitT {
    val symbol = "Zibps"
    val conversionFactor = BitsPerSecond.conversionFactor * ops.informationOps.Zebibytes.conversionFactor
  }

  object YottabitsPerSecond extends DataRateUnitT {
    val symbol = "Ybps"
    val conversionFactor = BitsPerSecond.conversionFactor * ops.informationOps.Yottabytes.conversionFactor
  }

  object YobibitsPerSecond extends DataRateUnitT {
    val symbol = "Yibps"
    val conversionFactor = BitsPerSecond.conversionFactor * ops.informationOps.Yobibytes.conversionFactor
  }

  object DataRateConversions {
    lazy val bytesPerSecond = BytesPerSecond(1)
    lazy val kilobytesPerSecond = KilobytesPerSecond(1)
    lazy val kibibytesPerSecond = KibibytesPerSecond(1)
    lazy val megabytesPerSecond = MegabytesPerSecond(1)
    lazy val mebibytesPerSecond = MebibytesPerSecond(1)
    lazy val gigabytesPerSecond = GigabytesPerSecond(1)
    lazy val gibibytesPerSecond = GibibytesPerSecond(1)
    lazy val terabytesPerSecond = TerabytesPerSecond(1)
    lazy val tebibytesPerSecond = TebibytesPerSecond(1)
    lazy val petabytesPerSecond = PetabytesPerSecond(1)
    lazy val pebibytesPerSecond = PebibytesPerSecond(1)
    lazy val exabytesPerSecond = ExabytesPerSecond(1)
    lazy val exbibytesPerSecond = ExbibytesPerSecond(1)
    lazy val zettabytesPerSecond = ZettabytesPerSecond(1)
    lazy val zebibytesPerSecond = ZebibytesPerSecond(1)
    lazy val yottabytesPerSecond = YottabytesPerSecond(1)
    lazy val yobibytesPerSecond = YobibytesPerSecond(1)

    lazy val bitsPerSecond = BitsPerSecond(1)
    lazy val kilobitsPerSecond = KilobitsPerSecond(1)
    lazy val kibibitsPerSecond = KibibitsPerSecond(1)
    lazy val megabitsPerSecond = MegabitsPerSecond(1)
    lazy val mebibitsPerSecond = MebibitsPerSecond(1)
    lazy val gigabitsPerSecond = GigabitsPerSecond(1)
    lazy val gibibitsPerSecond = GibibitsPerSecond(1)
    lazy val terabitsPerSecond = TerabitsPerSecond(1)
    lazy val tebibitsPerSecond = TebibitsPerSecond(1)
    lazy val petabitsPerSecond = PetabitsPerSecond(1)
    lazy val pebibitsPerSecond = PebibitsPerSecond(1)
    lazy val exabitsPerSecond = ExabitsPerSecond(1)
    lazy val exbibitsPerSecond = ExbibitsPerSecond(1)
    lazy val zettabitsPerSecond = ZettabitsPerSecond(1)
    lazy val zebibitsPerSecond = ZebibitsPerSecond(1)
    lazy val yottabitsPerSecond = YottabitsPerSecond(1)
    lazy val yobibitsPerSecond = YobibitsPerSecond(1)

    implicit class DataRateConversions[A](a: A)(implicit num: Numeric[A]) {
      def bytesPerSecond = BytesPerSecond(a)
      def kilobytesPerSecond = KilobytesPerSecond(a)
      def kibibytesPerSecond = KibibytesPerSecond(a)
      def megabytesPerSecond = MegabytesPerSecond(a)
      def mebibytesPerSecond = MebibytesPerSecond(a)
      def gigabytesPerSecond = GigabytesPerSecond(a)
      def gibibytesPerSecond = GibibytesPerSecond(a)
      def terabytesPerSecond = TerabytesPerSecond(a)
      def tebibytesPerSecond = TebibytesPerSecond(a)
      def petabytesPerSecond = PetabytesPerSecond(a)
      def pebibytesPerSecond = PebibytesPerSecond(a)
      def exabytesPerSecond = ExabytesPerSecond(a)
      def exbibytesPerSecond = ExbibytesPerSecond(a)
      def zettabytesPerSecond = ZettabytesPerSecond(a)
      def zebibytesPerSecond = ZebibytesPerSecond(a)
      def yottabytesPerSecond = YottabytesPerSecond(a)
      def yobibytesPerSecond = YobibytesPerSecond(a)

      def bitsPerSecond = BitsPerSecond(a)
      def kilobitsPerSecond = KilobitsPerSecond(a)
      def kibibitsPerSecond = KibibitsPerSecond(a)
      def megabitsPerSecond = MegabitsPerSecond(a)
      def mebibitsPerSecond = MebibitsPerSecond(a)
      def gigabitsPerSecond = GigabitsPerSecond(a)
      def gibibitsPerSecond = GibibitsPerSecond(a)
      def terabitsPerSecond = TerabitsPerSecond(a)
      def tebibitsPerSecond = TebibitsPerSecond(a)
      def petabitsPerSecond = PetabitsPerSecond(a)
      def pebibitsPerSecond = PebibitsPerSecond(a)
      def exabitsPerSecond = ExabitsPerSecond(a)
      def exbibitsPerSecond = ExbibitsPerSecond(a)
      def zettabitsPerSecond = ZettabitsPerSecond(a)
      def zebibitsPerSecond = ZebibitsPerSecond(a)
      def yottabitsPerSecond = YottabitsPerSecond(a)
      def yobibitsPerSecond = YobibitsPerSecond(a)
    }
  }
}
