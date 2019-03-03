
package org.terra

/**
  * The types used by the CommonTerraOps version of Terra.  Specifies use of
  * <ul>
  * <li>Double for floating point operations,</li>
  * <li>Long for whole number operations (Information),</li>
  * <li>BigDecimal for currency operation, and</li>
  * <li>Long for time operations</li></ul>
  */
class CommonTuple extends TypeContext {
  type T = Double
  type TL = Long
  type TC = BigDecimal
  type TT = Long
}

package object common extends TypeScope[CommonTuple] {

  /**
    * The TerraOps object for the common tree.  Provides conversion and math
    * functions for the CommonTuple types: Double, Long, BigDecimal, and Long
    */
  implicit object CommonTerraOps extends AbstractDoubleTerraOps[CommonTuple] {

    implicit val num: Fractional[T] = doubleNumeric
    implicit val numL: Numeric[TL] = longNumeric
    implicit val numC: Fractional[TC] = bigDecimalNumeric
    implicit val numT: Numeric[TT] = longNumeric

    override def getClassTagTT: PseudoClassTag[TT] =
      getClassTagTL.asInstanceOf[PseudoClassTag[TT]]
      //LongTag.asInstanceOf[ClassTag[TT]]

    val converters = CommonConverters

    object CommonConverters extends Converters[CommonTuple] {

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
        override def conv(in: T): TL = scala.math.round(in)
      }
      implicit val tttConverter = new HasConverter[T, TT] {
        override def conv(in: T): TT = scala.math.round(in)
      }
      implicit val ttcConverter = new HasConverter[T, TC] {
        override def conv(in: T): TC = BigDecimal(in)
      }
      implicit val tltConverter = new HasConverter[TL, T] {
        override def conv(in: TL): T = in.toDouble
      }
      implicit val tlttConverter = new HasConverter[TL, TT] {
        override def conv(in: TL): TT = in
      }
      implicit val tltcConverter = new HasConverter[TL, TC] {
        override def conv(in: TL): TC = BigDecimal(in.toDouble)
      }
      implicit val tttlConverter = new HasConverter[TT, TL] {
        override def conv(in: TT): TL = in
      }
      implicit val tttConverter2 = new HasConverter[TT, T] {
        override def conv(in: TT): T = in.toDouble
      }
      implicit val tttcConverter = new HasConverter[TT, TC] {
        override def conv(in: TT): TC = BigDecimal(in)
      }

      implicit val tctConverter = new HasConverter[TC, T] {
        override def conv(in: TC): T = in.doubleValue
      }
      implicit val tctlConverter = new HasConverter[TC, TL] {
        override def conv(in: TC): TL = in.longValue
      }
      implicit val tcttConverter = new HasConverter[TC, TT] {
        override def conv(in: TC): TT = in.longValue
      }

      implicit val dtConverter = new HasConverter[Double, T] {
        override def conv(in: Double): T = in
      }
      implicit val dtlConverter = new HasConverter[Double, TL] {
        override def conv(in: Double): TL = scala.math.round(in)
      }
      implicit val dttConverter = new HasConverter[Double, TT] {
        override def conv(in: Double): TT = scala.math.round(in)
      }
      implicit val dtcConverter = new HasConverter[Double, TC] {
        override def conv(in: Double): TC = BigDecimal(in)
      }

      implicit val ltConverter = new HasConverter[Long, T] {
        override def conv(in: Long): T = in.toDouble
      }
      implicit val ltlConverter = new HasConverter[Long, TL] {
        override def conv(in: Long): TL = in
      }
      implicit val lttConverter = new HasConverter[Long, TT] {
        override def conv(in: Long): TT = in
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
      implicit val ensureTL = new HasEnsureType[TL] {
        override def ensureType(in: Any): TL = in match {
          case b: Byte => b.toLong
          case s: Short => s.toLong
          case i: Int => i.toLong
          case l: Long => l
          case f: Float => scala.math.round(f).toLong
          case d: Double => scala.math.round(d).toLong
          case bd: BigDecimal => bd.toLong
          case bi: BigInt => bi.longValue
          case o => throw new Exception(
            "unknown type " + o + " type=" + o.getClass.getName)
        }
      }
      implicit val ensureTT = ensureTL
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

    override def conv(d: T): TL = scala.math.round(d)
    override def rconv(l: TL): T = l.toDouble
    override def convL(d: TT): TL = d
    override def rconvL(l: TL): TT = l
    override def convT(d: T): TT = scala.math.round(d)
    override def rconvT(d: TT): T = d.toDouble
  }

  implicit val ops = CommonTerraOps
  implicit val tag = ops.getClassTagT
  implicit val dtag = tag.asInstanceOf[PseudoClassTag[Double]]

  trait SymbolMixin {
    implicit val ops: TerraOps[CommonTuple] = CommonTerraOps
  }

  object information extends SymbolMixin 
      with org.terra.information.InformationSymbols[CommonTuple]
  object time extends SymbolMixin 
      with org.terra.time.TimeSymbols[CommonTuple]
  object electro extends SymbolMixin 
      with org.terra.electro.ElectroSymbols[CommonTuple]
  object energy extends SymbolMixin 
      with org.terra.energy.EnergySymbols[CommonTuple]
  object mass extends SymbolMixin 
      with org.terra.mass.MassSymbols[CommonTuple]
  object motion extends SymbolMixin 
      with org.terra.motion.MotionSymbols[CommonTuple]
  object photo extends SymbolMixin 
      with org.terra.photo.PhotoSymbols[CommonTuple]
  object radio extends SymbolMixin 
      with org.terra.radio.RadioSymbols[CommonTuple]
  object space extends SymbolMixin 
      with org.terra.space.SpaceSymbols[CommonTuple]
  object thermal extends SymbolMixin
      with org.terra.thermal.ThermalSymbols[CommonTuple]
  object market extends SymbolMixin
      with org.terra.market.MarketSymbols[CommonTuple] 

  implicit class QuantityDoubleT(d: Double)
      extends QuantityHelper[CommonTuple#T, Double](
    d, Numeric.DoubleIsFractional) {

    def *[A <: Quantity[A, CommonTuple#T, CommonTuple]](that: A): A = 
      times(that)
    def *[A <: Quantity[A, CommonTuple#T, CommonTuple]](
      that: SVector[A, CommonTuple#T, CommonTuple]): 
        SVector[A, CommonTuple#T, CommonTuple] =
      times(that)
    def /(that: Time): Frequency = div(that)
    def per(that: Time): Frequency = div(that)
  }

  implicit class QuantityDoubleTL(d: Double) 
      extends QuantityHelper[CommonTuple#TL, Double](
    d, Numeric.DoubleIsFractional) {
    def *[A <: Quantity[A, CommonTuple#TL, CommonTuple]](that: A): A = 
      times(that)
    def *[A <: Quantity[A, CommonTuple#TL, CommonTuple]](
      that: SVector[A, CommonTuple#TL, CommonTuple]): 
        SVector[A, CommonTuple#TL, CommonTuple] =
      times(that)
  }

  implicit class QuantityLongT(l: Long)
      extends QuantityHelper[CommonTuple#T, Long](l, Numeric.LongIsIntegral) {

    def *[A <: Quantity[A, CommonTuple#T, CommonTuple]](that: A): A = 
      times(that)
    def *[A <: Quantity[A, CommonTuple#T, CommonTuple]](
      that: SVector[A, CommonTuple#T, CommonTuple]): 
        SVector[A, CommonTuple#T, CommonTuple] = 
      times(that)
    def /(that: Time): Frequency = div(that)
    def per(that: Time): Frequency = div(that)
  }

  implicit class QuantityLongTL(l: Long) 
    extends QuantityHelper[CommonTuple#TL, Long](l, Numeric.LongIsIntegral) {

    def *[A <: Quantity[A, CommonTuple#TL, CommonTuple]](that: A): A =
      times(that)
    def *[A <: Quantity[A, CommonTuple#TL, CommonTuple]](
      that: SVector[A, CommonTuple#TL, CommonTuple]): 
        SVector[A, CommonTuple#TL, CommonTuple] =
      times(that)
  }

  //implicit class QuantityLongTT(l: Long) extends QuantityDoubleTT(l.toDouble)

  implicit class QuantityIntT(i: Int) 
      extends QuantityHelper[CommonTuple#T, Int](i, Numeric.IntIsIntegral) {

    def *[A <: Quantity[A, CommonTuple#T, CommonTuple]](that: A): A = 
      times(that)
    def *[A <: Quantity[A, CommonTuple#T, CommonTuple]](
      that: SVector[A, CommonTuple#T, CommonTuple]): 
        SVector[A, CommonTuple#T, CommonTuple] =
      times(that)
    def /(that: Time): Frequency = div(that)
    def per(that: Time): Frequency = div(that)
  }

  implicit class QuantityIntTL(i: Int) 
      extends QuantityHelper[CommonTuple#TL, Int](i, Numeric.IntIsIntegral) {

    def *[A <: Quantity[A, CommonTuple#TL, CommonTuple]](that: A): A = 
      times(that)
    def *[A <: Quantity[A, CommonTuple#TL, CommonTuple]](
      that: SVector[A, CommonTuple#TL, CommonTuple]): 
        SVector[A, CommonTuple#TL, CommonTuple] =
      times(that)
  }

  //implicit class QuantityIntTT(i: Int) extends QuantityDoubleTT(i.toDouble)

  implicit class QuantityBigDecimalT(bd: BigDecimal) 
      extends QuantityHelper[CommonTuple#T, BigDecimal](
    bd, Numeric.BigDecimalIsFractional) {

    def *[A <: Quantity[A, CommonTuple#T, CommonTuple]](that: A): A = 
      times(that)
    def *[A <: Quantity[A, CommonTuple#T, CommonTuple]](
      that: SVector[A, CommonTuple#T, CommonTuple]): 
        SVector[A, CommonTuple#T, CommonTuple] =
      times(that)
    def /(that: Time): Frequency = div(that)
    def per(that: Time): Frequency = div(that)
  }

  implicit class QuantityBigDecimalTL(bd: BigDecimal) 
      extends QuantityHelper[CommonTuple#TL, BigDecimal](
    bd, Numeric.BigDecimalIsFractional) {

    def *[A <: Quantity[A, CommonTuple#TL, CommonTuple]](that: A): A = 
      times(that)
    def *[A <: Quantity[A, CommonTuple#TL, CommonTuple]](
      that: SVector[A, CommonTuple#TL, CommonTuple]): 
        SVector[A, CommonTuple#TL, CommonTuple] =
      times(that)
  }
  
  implicit class QuantityBigDecimalTC(bd: BigDecimal) 
      extends QuantityHelper[CommonTuple#TC, BigDecimal](
    bd, Numeric.BigDecimalIsFractional) {

    def *[A <: Quantity[A, CommonTuple#TC, CommonTuple]](that: A): A = 
      times(that)
    def *[A <: Quantity[A, CommonTuple#TC, CommonTuple]](
      that: SVector[A, CommonTuple#TC, CommonTuple]): 
        SVector[A, CommonTuple#TC, CommonTuple] =
      times(that)
  }

  //implicit class QuantityBigDecimalTT(i: Int) extends QuantityHelper(i.toDouble)
}
