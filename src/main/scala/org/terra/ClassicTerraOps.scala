
package org.terra

/**
  * The types used by the ClassicTerraOps version of Terra.  Specifies use of
  * <ul>
  * <li>Double for floating point operations,</li>
  * <li>Long for whole number operations (Information),</li>
  * <li>BigDecimal for currency operation, and</li>
  * <li>Double for time operations</li></ul>
  */
class ClassicTuple extends TypeContext {
  type T = Double
  type TL = Double
  type TC = BigDecimal
  type TT = Double
}

package object classic extends TypeScope[ClassicTuple] {

  /**
    * The TerraOps object for the classic tree.  Provides conversion and math
    * functions for the ClassicTuple types: Double, Long, BigDecimal, and 
    * Double
    */
  implicit object ClassicTerraOps
      extends AbstractDoubleTerraOps[ClassicTuple] {

    implicit val num: Numeric[T] = doubleNumeric
    implicit val numL: Numeric[TL] = doubleNumeric
    implicit val numC: Numeric[TC] = bigDecimalNumeric
    implicit val numT: Numeric[TT] = doubleNumeric

    val converters = ClassicConverters

    object ClassicConverters extends Converters[ClassicTuple] {

      implicit val doubleConverter = new HasConverter[Double, Long] {
        override def conv(in: Double): Long = scala.math.round(in)
      }
      implicit val longIdConverter = new HasConverter[Long, Long] {
        override def conv(in: Long): Long = in
      }
      implicit val longConverter = new HasConverter[Long, Double] {
        override def conv(in: Long): Double = in.toDouble
      }

      implicit val ttlConverter = new HasConverter[T, TL] {
        override def conv(in: T): TL = in
      }
      implicit val tttConverter = new HasConverter[T, TT] {
        override def conv(in: T): TT = in
      }
      implicit val ttcConverter = new HasConverter[T, TC] {
        override def conv(in: T): TC = BigDecimal(in)
      }
      implicit val tltConverter = new HasConverter[TL, T] {
        override def conv(in: TL): T = in
      }
      implicit val tlttConverter = new HasConverter[TL, TT] {
        override def conv(in: TL): TT = in
      }
      implicit val tltcConverter = new HasConverter[TL, TC] {
        override def conv(in: TL): TC = BigDecimal(in)
      }
      implicit val tttlConverter = new HasConverter[TT, TL] {
        override def conv(in: TT): TL = in
      }
      implicit val tttConverter2 = new HasConverter[TT, T] {
        override def conv(in: TT): T = in
      }
      implicit val tttcConverter = new HasConverter[TT, TC] {
        override def conv(in: TT): TC = BigDecimal(in)
      }
      implicit val tctConverter = new HasConverter[TC, T] {
        override def conv(in: TC): T = in.doubleValue
      }
      implicit val tctlConverter = new HasConverter[TC, TL] {
        override def conv(in: TC): TL = in.doubleValue
      }
      implicit val tcttConverter = new HasConverter[TC, TT] {
        override def conv(in: TC): TT = in.doubleValue
      }

      implicit val dtConverter = new HasConverter[Double, T] {
        override def conv(in: Double): T = in
      }
      implicit val dtlConverter = new HasConverter[Double, TL] {
        override def conv(in: Double): TL = in
      }
      implicit val dttConverter = new HasConverter[Double, TT] {
        override def conv(in: Double): TT = in
      }
      implicit val dtcConverter = new HasConverter[Double, TC] {
        override def conv(in: Double): TC = BigDecimal(in)
      }

      implicit val ltConverter = new HasConverter[Long, T] {
        override def conv(in: Long): T = in.toDouble
      }
      implicit val ltlConverter = new HasConverter[Long, TL] {
        override def conv(in: Long): TL = in.toDouble
      }
      implicit val lttConverter = new HasConverter[Long, TT] {
        override def conv(in: Long): TT = in.toDouble
      }
      implicit val ltcConverter = new HasConverter[Long, TC] {
        override def conv(in: Long): TC = BigDecimal(in)
      }
      implicit val bdtcConverter = new HasConverter[BigDecimal, TC] {
        override def conv(in: BigDecimal): TC = in
      }

      implicit val ensureT = new HasEnsureType[T] {
        override def ensureType(in: Any): T = in match {
          case b: Byte => b.toDouble
          case s: Short => s.toDouble
          case i: Int => i.toDouble
          case l: Long => l.toDouble
          case f: Float => f.toDouble
          case d: Double => d
          case bd: BigDecimal => bd.toDouble
          case bi: BigInt => bi.toDouble
          case o => throw new Exception(
            "unknown type " + o + " type=" + o.getClass.getName)
        }
      }
      implicit val ensureTL = ensureT
      implicit val ensureTT = ensureT
      implicit val ensureTC = new HasEnsureType[TC] {
        override def ensureType(in: Any): TC = in match {
          case b: Byte => BigDecimal(b.toDouble)
          case s: Short => BigDecimal(s.toDouble)
          case i: Int => BigDecimal(i.toDouble)
          case l: Long => BigDecimal(l.toDouble)
          case f: Float => BigDecimal(f.toDouble)
          case d: Double => BigDecimal(d)
          case bd: BigDecimal => bd
          case bi: BigInt => BigDecimal(bi.doubleValue)
          case o => throw new Exception(
            "unknown type " + o + " type=" + o.getClass.getName)
        }
      }
    }

    def makeEnsureType[T1](test: Any): HasEnsureType[T1] = test match {
      case d: Double => converters.ensureT.asInstanceOf[HasEnsureType[T1]]
      case f: Float => converters.ensureT.asInstanceOf[HasEnsureType[T1]]
      case l: Long => converters.ensureTL.asInstanceOf[HasEnsureType[T1]]
      case i: Int => converters.ensureTL.asInstanceOf[HasEnsureType[T1]]
      case s: Short => converters.ensureTL.asInstanceOf[HasEnsureType[T1]]
      case b: Byte => converters.ensureTL.asInstanceOf[HasEnsureType[T1]]
      case bd: BigDecimal => converters.ensureTC.asInstanceOf[HasEnsureType[T1]]
    }

    override def conv(d: T): TL = d
    override def rconv(l: TL): T = l
    override def convL(d: TT): TL = d
    override def rconvL(l: TL): TT = l
    override def convT(d: T): TT = d
    override def rconvT(d: TT): T = d
  }

  implicit val ops = ClassicTerraOps

  trait SymbolMixin {
    implicit val ops: TerraOps[ClassicTuple] = ClassicTerraOps
  }

  object information extends SymbolMixin 
      with org.terra.information.InformationSymbols[ClassicTuple]
  object time extends SymbolMixin 
      with org.terra.time.TimeSymbols[ClassicTuple]
  object electro extends SymbolMixin 
      with org.terra.electro.ElectroSymbols[ClassicTuple]
  object energy extends SymbolMixin 
      with org.terra.energy.EnergySymbols[ClassicTuple]
  object mass extends SymbolMixin 
      with org.terra.mass.MassSymbols[ClassicTuple]
  object motion extends SymbolMixin 
      with org.terra.motion.MotionSymbols[ClassicTuple]
  object photo extends SymbolMixin 
      with org.terra.photo.PhotoSymbols[ClassicTuple]
  object radio extends SymbolMixin 
      with org.terra.radio.RadioSymbols[ClassicTuple]
  object space extends SymbolMixin 
      with org.terra.space.SpaceSymbols[ClassicTuple]
  object thermal extends SymbolMixin 
      with org.terra.thermal.ThermalSymbols[ClassicTuple]
  object market extends SymbolMixin
      with org.terra.market.MarketSymbols[ClassicTuple]

  implicit class QuantityDoubleT(d: Double)
      extends QuantityHelper[ClassicTuple#T, Double](
    d, Numeric.DoubleIsFractional) {

    def *[A <: Quantity[A, ClassicTuple#T, ClassicTuple]](that: A): A = 
      times(that)
    def *[A <: Quantity[A, ClassicTuple#T, ClassicTuple]](
      that: SVector[A, ClassicTuple#T, ClassicTuple]): 
        SVector[A, ClassicTuple#T, ClassicTuple] =
      times(that)
    def /(that: Time): Frequency = div(that)
    def per(that: Time): Frequency = div(that)
  }

  implicit class QuantityDoubleTL(d: Double) 
      extends QuantityHelper[ClassicTuple#TL, Double](
    d, Numeric.DoubleIsFractional) {
    def *[A <: Quantity[A, ClassicTuple#TL, ClassicTuple]](that: A): A = 
      times(that)
    def *[A <: Quantity[A, ClassicTuple#TL, ClassicTuple]](
      that: SVector[A, ClassicTuple#TL, ClassicTuple]): 
        SVector[A, ClassicTuple#TL, ClassicTuple] =
      times(that)
  }

  implicit class QuantityLongT(l: Long)
      extends QuantityHelper[ClassicTuple#T, Long](l, Numeric.LongIsIntegral) {

    def *[A <: Quantity[A, ClassicTuple#T, ClassicTuple]](that: A): A = 
      times(that)
    def *[A <: Quantity[A, ClassicTuple#T, ClassicTuple]](
      that: SVector[A, ClassicTuple#T, ClassicTuple]): 
        SVector[A, ClassicTuple#T, ClassicTuple] = 
      times(that)
    def /(that: Time): Frequency = div(that)
    def per(that: Time): Frequency = div(that)
  }

  implicit class QuantityLongTL(l: Long) 
    extends QuantityHelper[ClassicTuple#TL, Long](l, Numeric.LongIsIntegral) {

    def *[A <: Quantity[A, ClassicTuple#TL, ClassicTuple]](that: A): A =
      times(that)
    def *[A <: Quantity[A, ClassicTuple#TL, ClassicTuple]](
      that: SVector[A, ClassicTuple#TL, ClassicTuple]): 
        SVector[A, ClassicTuple#TL, ClassicTuple] =
      times(that)
  }

  //implicit class QuantityLongTT(l: Long) extends QuantityDoubleTT(l.toDouble)

  implicit class QuantityIntT(i: Int) 
      extends QuantityHelper[ClassicTuple#T, Int](i, Numeric.IntIsIntegral) {

    def *[A <: Quantity[A, ClassicTuple#T, ClassicTuple]](that: A): A = 
      times(that)
    def *[A <: Quantity[A, ClassicTuple#T, ClassicTuple]](
      that: SVector[A, ClassicTuple#T, ClassicTuple]): 
        SVector[A, ClassicTuple#T, ClassicTuple] =
      times(that)
    def /(that: Time): Frequency = div(that)
    def per(that: Time): Frequency = div(that)
  }

  implicit class QuantityIntTL(i: Int) 
      extends QuantityHelper[ClassicTuple#TL, Int](i, Numeric.IntIsIntegral) {

    def *[A <: Quantity[A, ClassicTuple#TL, ClassicTuple]](that: A): A = 
      times(that)
    def *[A <: Quantity[A, ClassicTuple#TL, ClassicTuple]](
      that: SVector[A, ClassicTuple#TL, ClassicTuple]): 
        SVector[A, ClassicTuple#TL, ClassicTuple] =
      times(that)
  }

  //implicit class QuantityIntTT(i: Int) extends QuantityDoubleTT(i.toDouble)

  implicit class QuantityBigDecimalT(bd: BigDecimal) 
      extends QuantityHelper[ClassicTuple#T, BigDecimal](
    bd, Numeric.BigDecimalIsFractional) {

    def *[A <: Quantity[A, ClassicTuple#T, ClassicTuple]](that: A): A = 
      times(that)
    def *[A <: Quantity[A, ClassicTuple#T, ClassicTuple]](
      that: SVector[A, ClassicTuple#T, ClassicTuple]): 
        SVector[A, ClassicTuple#T, ClassicTuple] =
      times(that)
    def /(that: Time): Frequency = div(that)
    def per(that: Time): Frequency = div(that)
  }

  implicit class QuantityBigDecimalTL(bd: BigDecimal) 
      extends QuantityHelper[ClassicTuple#TL, BigDecimal](
    bd, Numeric.BigDecimalIsFractional) {

    def *[A <: Quantity[A, ClassicTuple#TL, ClassicTuple]](that: A): A = 
      times(that)
    def *[A <: Quantity[A, ClassicTuple#TL, ClassicTuple]](
      that: SVector[A, ClassicTuple#TL, ClassicTuple]): 
        SVector[A, ClassicTuple#TL, ClassicTuple] =
      times(that)
  }
  
  implicit class QuantityBigDecimalTC(bd: BigDecimal) 
      extends QuantityHelper[ClassicTuple#TC, BigDecimal](
    bd, Numeric.BigDecimalIsFractional) {

    def *[A <: Quantity[A, ClassicTuple#TC, ClassicTuple]](that: A): A = 
      times(that)
    def *[A <: Quantity[A, ClassicTuple#TC, ClassicTuple]](
      that: SVector[A, ClassicTuple#TC, ClassicTuple]): 
        SVector[A, ClassicTuple#TC, ClassicTuple] =
      times(that)
  }

  //implicit class QuantityBigDecimalTT(i: Int) extends QuantityHelper(i.toDouble)
}
