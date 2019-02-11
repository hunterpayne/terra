
package org.terra
package information

/**
  * Contains all the symbols exported by the org.terra.*.information packages.
  * Synthetic packages then subclass this trait with an object to represent
  * a package in the synthetic trees of symbols used by the unit test and
  * user's calling code.
  * @author Hunter Payne
  */
trait InformationSymbols[Tuple <: TypeContext] {

  implicit val ops: TerraOps[Tuple]

  type Information = InformationLike[Tuple]
  lazy val Bytes = ops.informationOps.Bytes
  lazy val Kilobytes = ops.informationOps.Kilobytes
  lazy val Kibibytes = ops.informationOps.Kibibytes
  lazy val Megabytes = ops.informationOps.Megabytes
  lazy val Mebibytes = ops.informationOps.Mebibytes
  lazy val Gigabytes = ops.informationOps.Gigabytes
  lazy val Gibibytes = ops.informationOps.Gibibytes
  lazy val Terabytes = ops.informationOps.Terabytes
  lazy val Tebibytes = ops.informationOps.Tebibytes
  lazy val Petabytes = ops.informationOps.Petabytes
  lazy val Pebibytes = ops.informationOps.Pebibytes
  lazy val Exabytes = ops.informationOps.Exabytes
  lazy val Exbibytes = ops.informationOps.Exbibytes
  lazy val Zettabytes = ops.informationOps.Zettabytes
  lazy val Zebibytes = ops.informationOps.Zebibytes
  lazy val Yottabytes = ops.informationOps.Yottabytes
  lazy val Yobibytes = ops.informationOps.Yobibytes
  lazy val Bits = ops.informationOps.Bits
  lazy val Kilobits = ops.informationOps.Kilobits
  lazy val Kibibits = ops.informationOps.Kibibits
  lazy val Megabits = ops.informationOps.Megabits
  lazy val Mebibits = ops.informationOps.Mebibits
  lazy val Gigabits = ops.informationOps.Gigabits
  lazy val Gibibits = ops.informationOps.Gibibits
  lazy val Terabits = ops.informationOps.Terabits
  lazy val Tebibits = ops.informationOps.Tebibits
  lazy val Petabits = ops.informationOps.Petabits
  lazy val Pebibits = ops.informationOps.Pebibits
  lazy val Exabits = ops.informationOps.Exabits
  lazy val Exbibits = ops.informationOps.Exbibits
  lazy val Zettabits = ops.informationOps.Zettabits
  lazy val Zebibits = ops.informationOps.Zebibits
  lazy val Yottabits = ops.informationOps.Yottabits
  lazy val Yobibits = ops.informationOps.Yobibits
  lazy val Information = ops.informationOps.Information

  object InformationConversions {

    import ops.informationOps.{ InformationConversions => Convs }

    lazy val byte = Convs.byte
    lazy val kilobyte = Convs.kilobyte
    lazy val kibibyte = Convs.kibibyte
    lazy val megabyte = Convs.megabyte
    lazy val mebibyte = Convs.mebibyte
    lazy val gigabyte = Convs.gigabyte
    lazy val gibibyte = Convs.gibibyte
    lazy val terabyte = Convs.terabyte
    lazy val tebibyte = Convs.tebibyte
    lazy val petabyte = Convs.petabyte
    lazy val pebibyte = Convs.pebibyte
    lazy val exabyte = Convs.exabyte
    lazy val exbibyte = Convs.exbibyte
    lazy val zettabyte = Convs.zettabyte
    lazy val zebibyte = Convs.zebibyte
    lazy val yottabyte = Convs.yottabyte
    lazy val yobibyte = Convs.yobibyte
    lazy val bit = Convs.bit
    lazy val kilobit = Convs.kilobit
    lazy val kibibit = Convs.kibibit
    lazy val megabit = Convs.megabit
    lazy val mebibit = Convs.mebibit
    lazy val gigabit = Convs.gigabit
    lazy val gibibit = Convs.gibibit
    lazy val terabit = Convs.terabit
    lazy val tebibit = Convs.tebibit
    lazy val petabit = Convs.petabit
    lazy val pebibit = Convs.pebibit
    lazy val exabit = Convs.exabit
    lazy val exbibit = Convs.exbibit
    lazy val zettabit = Convs.zettabit
    lazy val zebibit = Convs.zebibit
    lazy val yottabit = Convs.yottabit
    lazy val yobibit = Convs.yobibit

    implicit class InformationConversions[B](a: B)(implicit num: Numeric[B])
        extends Convs.InformationConversions[B](a)

    implicit class InformationStringConversions(s: String)
        extends Convs.InformationStringConversions(s)

    type InformationT = InformationLike[Tuple]
    type PrimaryUnitT = PrimaryUnit[Tuple#TL, Tuple]
    type UnitOfMeasureT = UnitOfMeasure[InformationT, Tuple#TL, Tuple]
    implicit object InformationNumeric
        extends AbstractQuantityNumericL[InformationT, Tuple](Information)
  }

  type DataRate = DataRateLike[Tuple]
  lazy val BytesPerSecond = ops.dataRateOps.BytesPerSecond
  lazy val KilobytesPerSecond = ops.dataRateOps.KilobytesPerSecond
  lazy val KibibytesPerSecond = ops.dataRateOps.KibibytesPerSecond
  lazy val MegabytesPerSecond = ops.dataRateOps.MegabytesPerSecond
  lazy val MebibytesPerSecond = ops.dataRateOps.MebibytesPerSecond
  lazy val GigabytesPerSecond = ops.dataRateOps.GigabytesPerSecond
  lazy val GibibytesPerSecond = ops.dataRateOps.GibibytesPerSecond
  lazy val TerabytesPerSecond = ops.dataRateOps.TerabytesPerSecond
  lazy val TebibytesPerSecond = ops.dataRateOps.TebibytesPerSecond
  lazy val PetabytesPerSecond = ops.dataRateOps.PetabytesPerSecond
  lazy val PebibytesPerSecond = ops.dataRateOps.PebibytesPerSecond
  lazy val ExabytesPerSecond = ops.dataRateOps.ExabytesPerSecond
  lazy val ExbibytesPerSecond = ops.dataRateOps.ExbibytesPerSecond
  lazy val ZettabytesPerSecond = ops.dataRateOps.ZettabytesPerSecond
  lazy val ZebibytesPerSecond = ops.dataRateOps.ZebibytesPerSecond
  lazy val YottabytesPerSecond = ops.dataRateOps.YottabytesPerSecond
  lazy val YobibytesPerSecond = ops.dataRateOps.YobibytesPerSecond
  lazy val BitsPerSecond = ops.dataRateOps.BitsPerSecond
  lazy val KilobitsPerSecond = ops.dataRateOps.KilobitsPerSecond
  lazy val KibibitsPerSecond = ops.dataRateOps.KibibitsPerSecond
  lazy val MegabitsPerSecond = ops.dataRateOps.MegabitsPerSecond
  lazy val MebibitsPerSecond = ops.dataRateOps.MebibitsPerSecond
  lazy val GigabitsPerSecond = ops.dataRateOps.GigabitsPerSecond
  lazy val GibibitsPerSecond = ops.dataRateOps.GibibitsPerSecond
  lazy val TerabitsPerSecond = ops.dataRateOps.TerabitsPerSecond
  lazy val TebibitsPerSecond = ops.dataRateOps.TebibitsPerSecond
  lazy val PetabitsPerSecond = ops.dataRateOps.PetabitsPerSecond
  lazy val PebibitsPerSecond = ops.dataRateOps.PebibitsPerSecond
  lazy val ExabitsPerSecond = ops.dataRateOps.ExabitsPerSecond
  lazy val ExbibitsPerSecond = ops.dataRateOps.ExbibitsPerSecond
  lazy val ZettabitsPerSecond = ops.dataRateOps.ZettabitsPerSecond
  lazy val ZebibitsPerSecond = ops.dataRateOps.ZebibitsPerSecond
  lazy val YottabitsPerSecond = ops.dataRateOps.YottabitsPerSecond
  lazy val YobibitsPerSecond = ops.dataRateOps.YobibitsPerSecond
  lazy val DataRate = ops.dataRateOps.DataRate

  object DataRateConversions {

    import ops.dataRateOps.{ DataRateConversions => Convs }

    lazy val bytesPerSecond = Convs.bytesPerSecond
    lazy val kilobytesPerSecond = Convs.kilobytesPerSecond
    lazy val kibibytesPerSecond = Convs.kibibytesPerSecond
    lazy val megabytesPerSecond = Convs.megabytesPerSecond
    lazy val mebibytesPerSecond = Convs.mebibytesPerSecond
    lazy val gigabytesPerSecond = Convs.gigabytesPerSecond
    lazy val gibibytesPerSecond = Convs.gibibytesPerSecond
    lazy val terabytesPerSecond = Convs.terabytesPerSecond
    lazy val tebibytesPerSecond = Convs.tebibytesPerSecond
    lazy val petabytesPerSecond = Convs.petabytesPerSecond
    lazy val pebibytesPerSecond = Convs.pebibytesPerSecond
    lazy val exabytesPerSecond = Convs.exabytesPerSecond
    lazy val exbibytesPerSecond = Convs.exbibytesPerSecond
    lazy val zettabytesPerSecond = Convs.zettabytesPerSecond
    lazy val zebibytesPerSecond = Convs.zebibytesPerSecond
    lazy val yottabytesPerSecond = Convs.yottabytesPerSecond
    lazy val yobibytesPerSecond = Convs.yobibytesPerSecond

    lazy val bitsPerSecond = Convs.bitsPerSecond
    lazy val kilobitsPerSecond = Convs.kilobitsPerSecond
    lazy val kibibitsPerSecond = Convs.kibibitsPerSecond
    lazy val megabitsPerSecond = Convs.megabitsPerSecond
    lazy val mebibitsPerSecond = Convs.mebibitsPerSecond
    lazy val gigabitsPerSecond = Convs.gigabitsPerSecond
    lazy val gibibitsPerSecond = Convs.gibibitsPerSecond
    lazy val terabitsPerSecond = Convs.terabitsPerSecond
    lazy val tebibitsPerSecond = Convs.tebibitsPerSecond
    lazy val petabitsPerSecond = Convs.petabitsPerSecond
    lazy val pebibitsPerSecond = Convs.pebibitsPerSecond
    lazy val exabitsPerSecond = Convs.exabitsPerSecond
    lazy val exbibitsPerSecond = Convs.exbibitsPerSecond
    lazy val zettabitsPerSecond = Convs.zettabitsPerSecond
    lazy val zebibitsPerSecond = Convs.zebibitsPerSecond
    lazy val yottabitsPerSecond = Convs.yottabitsPerSecond
    lazy val yobibitsPerSecond = Convs.yobibitsPerSecond

    implicit object DataRateNumeric
        extends AbstractQuantityNumeric[DataRateLike[Tuple], Tuple](DataRate)

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
