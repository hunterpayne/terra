
package org.terra

/**
  * The types used by the DoubleTerraOps version of Terra.  Specifies use of
  * <ul>
  * <li>Double for floating point operations,</li>
  * <li>Double for whole number operations (Information),</li>
  * <li>Double for currency operation, and</li>
  * <li>Double for time operations</li></ul>
  */
class DoubleTuple extends TypeContext {
  type T = Double
  type TL = Double
  type TC = Double
  type TT = Double
}

package object double extends TypeScope[DoubleTuple] {

  /**
    * The TerraOps object for the double tree.  Provides conversion and math
    * functions for the DoubleTuple types which are all Doubles.  All the
    * conversions are identity.  Provides high precision and speed but will
    * yield surprising results at times when dealing with very large or
    * very small values at times.  Doesn't keep track of currencies correctly
    * so never use for actual finance.
    */
  implicit object DoubleTerraOps extends AbstractDoubleTerraOps[DoubleTuple] {

    implicit val num: Fractional[T] = doubleNumeric
    implicit val numL: Numeric[TL] = doubleNumeric
    implicit val numC: Fractional[TC] = doubleNumeric
    implicit val numT: Numeric[TT] = doubleNumeric

    val converters = DoubleConverters
    override def getClassTagTL: PseudoClassTag[TL] =
      getClassTagT.asInstanceOf[PseudoClassTag[TL]]
    //DoubleTag.asInstanceOf[ClassTag[TL]]

    object DoubleConverters extends Converters[DoubleTuple] {
      class IdConverter[A, B] extends HasConverter[A, B] {
        override def conv(in: A): B = in.asInstanceOf[B]
      }
      implicit val ttlConverter: HasConverter[T, TL] = new IdConverter
      implicit val tttConverter: HasConverter[T, TT] = new IdConverter
      implicit val ttcConverter: HasConverter[T, TC] = new IdConverter
      implicit val tltConverter: HasConverter[TL, T] = new IdConverter
      implicit val tlttConverter: HasConverter[TL, TT] = new IdConverter
      implicit val tltcConverter: HasConverter[TL, TC] = new IdConverter
      implicit val tttlConverter: HasConverter[TT, TL] = new IdConverter
      implicit val tttConverter2: HasConverter[TT, T] = new IdConverter
      implicit val tttcConverter: HasConverter[TT, TC] = new IdConverter
      implicit val tctConverter: HasConverter[TC, T] = new IdConverter
      implicit val tctlConverter: HasConverter[TC, TL] = new IdConverter
      implicit val tcttConverter: HasConverter[TC, TT] = new IdConverter

      implicit val dtConverter: HasConverter[Double, T] = new IdConverter
      implicit val dtlConverter: HasConverter[Double, TL] = new IdConverter
      implicit val dttConverter: HasConverter[Double, TT] = new IdConverter
      implicit val dtcConverter: HasConverter[Double, TC] = new IdConverter

      implicit val ltConverter: HasConverter[Long, T] =
        new HasConverter[Long, T] {
          override def conv(in: Long): T = in.toDouble
        }
      implicit val ltlConverter: HasConverter[Long, TL] =
        new HasConverter[Long, TL] {
          override def conv(in: Long): TL = in.toDouble
      }
      implicit val lttConverter: HasConverter[Long, TT] =
        new HasConverter[Long, TT] {
          override def conv(in: Long): TT = in.toDouble
        }
      implicit val ltcConverter: HasConverter[Long, TC] =
        new HasConverter[Long, TT] {
          override def conv(in: Long): TT = in.toDouble
        }
      implicit val bdtcConverter: HasConverter[BigDecimal, TC] =
        new HasConverter[BigDecimal, TC] {
          override def conv(in: BigDecimal): TC = in.doubleValue
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
      implicit val ensureTL: HasEnsureType[TL] =
        ensureT.asInstanceOf[HasEnsureType[TL]]
      implicit val ensureTT: HasEnsureType[TT] =
        ensureT.asInstanceOf[HasEnsureType[TL]]
      implicit val ensureTC: HasEnsureType[TC] =
        ensureT.asInstanceOf[HasEnsureType[TL]]
    }

    def makeEnsureType[T1](test: Any): HasEnsureType[T1] =
      converters.ensureT.asInstanceOf[HasEnsureType[T1]]
  }

  implicit val ops = DoubleTerraOps
  implicit val tag = ops.getClassTagT
  implicit val dtag = tag.asInstanceOf[PseudoClassTag[Double]]

  trait SymbolMixin {
    implicit val ops: TerraOps[DoubleTuple] = DoubleTerraOps
  }

  object information extends SymbolMixin 
      with org.terra.information.InformationSymbols[DoubleTuple]
  object time extends SymbolMixin 
      with org.terra.time.TimeSymbols[DoubleTuple]
  object electro extends SymbolMixin 
      with org.terra.electro.ElectroSymbols[DoubleTuple]
  object energy extends SymbolMixin 
      with org.terra.energy.EnergySymbols[DoubleTuple]
  object mass extends SymbolMixin 
      with org.terra.mass.MassSymbols[DoubleTuple]
  object motion extends SymbolMixin 
      with org.terra.motion.MotionSymbols[DoubleTuple]
  object photo extends SymbolMixin 
      with org.terra.photo.PhotoSymbols[DoubleTuple]
  object radio extends SymbolMixin 
      with org.terra.radio.RadioSymbols[DoubleTuple]
  object space extends SymbolMixin 
      with org.terra.space.SpaceSymbols[DoubleTuple]
  object thermal extends SymbolMixin 
      with org.terra.thermal.ThermalSymbols[DoubleTuple]
  object market extends SymbolMixin
      with org.terra.market.MarketSymbols[DoubleTuple] 

  implicit class QuantityDoubleT(d: Double)
      extends QuantityHelper[DoubleTuple#T, Double](
    d, Numeric.DoubleIsFractional) {

    def *[A <: Quantity[A, DoubleTuple#T, DoubleTuple]](that: A): A = 
      times(that)
    def *[A <: Quantity[A, DoubleTuple#T, DoubleTuple]](
      that: SVector[A, DoubleTuple#T, DoubleTuple]): 
        SVector[A, DoubleTuple#T, DoubleTuple] =
      times(that)
    def /(that: Time): Frequency = div(that)
    def per(that: Time): Frequency = div(that)
  }

  implicit class QuantityDoubleTL(d: Double) 
      extends QuantityHelper[DoubleTuple#TL, Double](
    d, Numeric.DoubleIsFractional) {
    def *[A <: Quantity[A, DoubleTuple#TL, DoubleTuple]](that: A): A = 
      times(that)
    def *[A <: Quantity[A, DoubleTuple#TL, DoubleTuple]](
      that: SVector[A, DoubleTuple#TL, DoubleTuple]): 
        SVector[A, DoubleTuple#TL, DoubleTuple] =
      times(that)
  }

  implicit class QuantityLongT(l: Long)
      extends QuantityHelper[DoubleTuple#T, Long](l, Numeric.LongIsIntegral) {

    def *[A <: Quantity[A, DoubleTuple#T, DoubleTuple]](that: A): A = 
      times(that)
    def *[A <: Quantity[A, DoubleTuple#T, DoubleTuple]](
      that: SVector[A, DoubleTuple#T, DoubleTuple]): 
        SVector[A, DoubleTuple#T, DoubleTuple] = 
      times(that)
    def /(that: Time): Frequency = div(that)
    def per(that: Time): Frequency = div(that)
  }

  implicit class QuantityLongTL(l: Long) 
    extends QuantityHelper[DoubleTuple#TL, Long](l, Numeric.LongIsIntegral) {

    def *[A <: Quantity[A, DoubleTuple#TL, DoubleTuple]](that: A): A =
      times(that)
    def *[A <: Quantity[A, DoubleTuple#TL, DoubleTuple]](
      that: SVector[A, DoubleTuple#TL, DoubleTuple]): 
        SVector[A, DoubleTuple#TL, DoubleTuple] =
      times(that)
  }

  //implicit class QuantityLongTT(l: Long) extends QuantityDoubleTT(l.toDouble)

  implicit class QuantityIntT(i: Int) 
      extends QuantityHelper[DoubleTuple#T, Int](i, Numeric.IntIsIntegral) {

    def *[A <: Quantity[A, DoubleTuple#T, DoubleTuple]](that: A): A = 
      times(that)
    def *[A <: Quantity[A, DoubleTuple#T, DoubleTuple]](
      that: SVector[A, DoubleTuple#T, DoubleTuple]): 
        SVector[A, DoubleTuple#T, DoubleTuple] =
      times(that)
    def /(that: Time): Frequency = div(that)
    def per(that: Time): Frequency = div(that)
  }

  implicit class QuantityIntTL(i: Int) 
      extends QuantityHelper[DoubleTuple#TL, Int](i, Numeric.IntIsIntegral) {

    def *[A <: Quantity[A, DoubleTuple#TL, DoubleTuple]](that: A): A = 
      times(that)
    def *[A <: Quantity[A, DoubleTuple#TL, DoubleTuple]](
      that: SVector[A, DoubleTuple#TL, DoubleTuple]): 
        SVector[A, DoubleTuple#TL, DoubleTuple] =
      times(that)
  }

  //implicit class QuantityIntTT(i: Int) extends QuantityDoubleTT(i.toDouble)

  implicit class QuantityBigDecimalT(bd: BigDecimal) 
      extends QuantityHelper[DoubleTuple#T, BigDecimal](
    bd, Numeric.BigDecimalIsFractional) {

    def *[A <: Quantity[A, DoubleTuple#T, DoubleTuple]](that: A): A = 
      times(that)
    def *[A <: Quantity[A, DoubleTuple#T, DoubleTuple]](
      that: SVector[A, DoubleTuple#T, DoubleTuple]): 
        SVector[A, DoubleTuple#T, DoubleTuple] =
      times(that)
    def /(that: Time): Frequency = div(that)
    def per(that: Time): Frequency = div(that)
  }

  implicit class QuantityBigDecimalTL(bd: BigDecimal) 
      extends QuantityHelper[DoubleTuple#TL, BigDecimal](
    bd, Numeric.BigDecimalIsFractional) {

    def *[A <: Quantity[A, DoubleTuple#TL, DoubleTuple]](that: A): A = 
      times(that)
    def *[A <: Quantity[A, DoubleTuple#TL, DoubleTuple]](
      that: SVector[A, DoubleTuple#TL, DoubleTuple]): 
        SVector[A, DoubleTuple#TL, DoubleTuple] =
      times(that)
  }
  
  implicit class QuantityBigDecimalTC(bd: BigDecimal) 
      extends QuantityHelper[DoubleTuple#TC, BigDecimal](
    bd, Numeric.BigDecimalIsFractional) {

    def *[A <: Quantity[A, DoubleTuple#TC, DoubleTuple]](that: A): A = 
      times(that)
    def *[A <: Quantity[A, DoubleTuple#TC, DoubleTuple]](
      that: SVector[A, DoubleTuple#TC, DoubleTuple]): 
        SVector[A, DoubleTuple#TC, DoubleTuple] =
      times(that)
  }

  //implicit class QuantityBigDecimalTT(i: Int) extends QuantityHelper(i.toDouble)
}
