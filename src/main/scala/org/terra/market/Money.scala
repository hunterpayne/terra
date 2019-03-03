/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package market

import scala.util.{Failure, Success, Try}
import scala.language.implicitConversions
import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal.RoundingMode.RoundingMode

/**
 * Represents a quantity of Money.
 *
 * Money is similar to other quantities in that it represents an amount of something - purchasing power - and
 * it is measured in units - currencies.
 *
 * The main difference is that the conversion rate between currencies can not be certain at compile.
 * (In fact it may not always be so easy to know them at runtime as well.)
 *
 * To address this diversion from the way most other quantities work, Money overrides several of the standard methods
 * and operators to ensure one of two rules is followed:
 *
 *  1) this and that are in the same currency, or
 *  2) there is in an implicit MoneyContext in scope (which may or may not have the applicable exchange rate)
 *
 * Methods and operations applied to moneys of different currencies may throw a NoSuchExchangeRateException if the
 * implicit MoneyContext does not contain the Rate(s) necessary to perform the conversion.
 *
 * The defaultMoneyContext includes USD as the default currency, a list of ~20 other currencies and NO exchange rates
 *
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value the amount of money
 * @param currency the currency in which the money is denominated
 */
final class MoneyLike[C <: TypeContext](
  val value: C#TC, val currency: CurrencyLike[C])(
  implicit ops: TerraOps[C], val context: MoneyContextLike[C])
    extends Quantity[MoneyLike[C], C#TC, C] {

  import ops.moneyOps._

  def dimension: Dimension[MoneyLike[C], C#TC, C] = {
    implicit val opsArg = ops
    new MoneyDim[C]
  }
  type Money = MoneyLike[C]
  type MoneyContext = MoneyContextLike[C]
  type Currency = CurrencyLike[C]

  override def getTag: PseudoClassTag[C#TC] = ops.getClassTagTC
  override def getNumeric: Numeric[C#TC] = ops.numC

  def unit = currency

  override def makeEnsureType(implicit ops: TerraOps[C]): HasEnsureType[C#TC] =
    ops.converters.ensureTC
  override def makeFromCurrencyConverter(
    implicit ops: TerraOps[C]): HasConverter[C#TC, C#TC] =
    new HasConverter[C#TC, C#TC] {
      def conv(d: C#TC): C#TC = d
    }
  override def makeToCurrencyConverter(
    implicit ops: TerraOps[C]): HasConverter[C#TC, C#TC] =
    new HasConverter[C#TC, C#TC] {
      def conv(d: C#TC): C#TC = d
    }

  /**
   * Returns a string formatted with the original precision amount and the currency code
   *
   * eg USD(123.456) => "123.456 USD"
   *
   * @return String
   */
  override def toString: String = {
    implicit val num: Numeric[C#TC] = ops.numC
    crossFormat[C#TC](value) + " " + currency.code
  }

  /**
    * Converts the amount to the given currency and returns a string formatted with the original precision and the currency code
    *
    * @param c Currency
    * @param context MoneyContext required for conversion
    * @return
    */
  def toString(c: Currency)(implicit context: MoneyContext): String = 
    in(c).toString

  /**
   * Returns a string formatted with the amount, rounded based on the Currency rules, and the currency symbol
   *
   * eg USD(12.4563) => "\$123.46"
   *
   * @return String
   */
  def toFormattedString: String = {
    implicit val e: HasEnsureType[C#TC] = ops.converters.ensureTC
    currency.symbol + ops.roundT[C#TC](
      value, currency.formatDecimals, BigDecimal.RoundingMode.HALF_EVEN)
  }

  def toFormattedString(c: Currency)(implicit context: MoneyContext): 
      String =
    in(c).toFormattedString

  /**
   * Multiplies this money by that BigDecimal and returns a new Money
   *
   * @param that BigDecimal
   * @return Money
   */
  def times(that: Double)(
    implicit ops: TerraOps[C], context: MoneyContext): Money =
    new Money(ops.numC.times(value, ops.convCurrency(that)), currency)

  /**
   * Multiplies this money by that [[org.terra.market.CurrencyExchangeRate]] and returns the equal value in the other currency.
   *
   * Delegates to CurrencyExchangeRate * Money
   *
   * @param that BigDecimal
   * @return
   */
  def *(that: CurrencyExchangeRate[C]): Money = that * this

  /**
   * Divide this money by another (non-money) Quantity and return a Price
   * @param that Quantity
   * @tparam A Quantity Type
   * @return Price[A]
   */
  def /[A <: Quantity[A, T, C], T](that: A)(
    implicit ops: TerraOps[C]): Price[A, T, C] =
    Price(this, that)

  /**
   * Divide this money by a Price and return Quantity
   * @param that Price
   * @tparam A Quantity Type
   * @return A
   */
  def /[A <: Quantity[A, T, C], T](that: Price[A, T, C])(
    implicit ops: TerraOps[C]): A = {
    implicit val e: HasEnsureType[T] = that.counter.makeEnsureType
    that.quantity * ops.ensureType[T](divide(that.money))
  }

  /**
   * Override for Quantity.equal to only match Moneys of like Currency
   * @param that Money must be of matching value and unit
   * @return
   */
  override def equals(that: Any): Boolean = {
    that match {
      case m: Money ⇒ {
        if (currency == m.currency) m.value == value
        else if (!m.context.rates.isEmpty) m.in(currency).value == value
        else false
      }
      case pm: ops.moneyOps.PseudoMoney1 => equals(pm.convert)
      case pm: ops.moneyOps.PseudoMoney2 => equals(pm.convert)
      case pm: ops.moneyOps.PseudoMoneyStr => equals(pm.convert)
      case _        ⇒ false
    }
  }

  def ==#(that: Money)(implicit moneyContext: MoneyContext) = 
    compare(that) == 0
  def !=#(that: Money)(implicit moneyContext: MoneyContext) = 
    compare(that) != 0
  def >#(that: Money)(implicit moneyContext: MoneyContext) = 
    compare(that) > 0
  def >=#(that: Money)(implicit moneyContext: MoneyContext) = 
    compare(that) >= 0
  def <#(that: Money)(implicit moneyContext: MoneyContext) = 
    compare(that) < 0
  def <=#(that: Money)(implicit moneyContext: MoneyContext) = 
    compare(that) <= 0

  /**
   * Combines with that Money to create an [[org.terra.market.CurrencyExchangeRate]]
   *
   * Exchange Rates on the same currency are not supported
   *
   * val rate: CurrencyExchangeRate = JPY(100) toThe USD(1)
   *
   * @param that Money
   * @return
   * @throws scala.IllegalArgumentException if the that.currency matches this.currency
   */
  def toThe(that: Money) = that.currency match {
    case this.currency ⇒ throw new IllegalArgumentException("Can not create Exchange Rate on matching currencies")
    case _             ⇒ CurrencyExchangeRate[C](that, this)
  }
  /**
   * toThe
   */
  def -> = toThe _

  /**
   * Convert this Money to a Double representing the currency unit
   *
   * @param unit Currency
   * @param context MoneyContext required for cross currency operations
   * @return Double
   * @throws NoSuchExchangeRateException when no exchange rate is available
   */
  override def to(unit: UnitOfMeasure[MoneyLike[C], C#TC, C]) =
    context.convert(this, unit.asInstanceOf[Currency]).value

  /**
   * Reboxes this Money value in a Money in the given Currency
   *
   * @param unit Currency
   * @param context MoneyContext required for cross currency operations
   * @return Money
   * @throws NoSuchExchangeRateException when no exchange rate is available
   */
  override def in(unit: UnitOfMeasure[MoneyLike[C], C#TC, C]) = 
    context.convert(this, unit.asInstanceOf[Currency])

  /**
    * Returns a Money rounded using scale and mode.
    *
    * @param scale Int - scale of the Money to be returned
    * @param mode RoundingMode - defaults to HALF_EVEN
    * @return Quantity
    */
  override def rounded(scale: Int, mode: RoundingMode = RoundingMode.HALF_EVEN) = {
    implicit val e: HasEnsureType[C#TC] = ops.converters.ensureTC
    implicit val num: Numeric[C#TC] = ops.numC
    new MoneyLike[C](ops.roundT[C#TC](value, scale, mode), currency)
  }

  override def hashCode() =
    java.util.Objects.hash(currency, ops.numC.toDouble(this.value).toString)
}

/**
 * Represents a Currency, which is the Unit of Measure for Money
 *
 * @param code Currency code
 * @param name Currency name
 * @param symbol Currency symbol
 * @param formatDecimals Number of decimals in standard formatting
 */
case class CurrencyLike[C <: TypeContext](
  val code: String, val name: String, val symbol: String, 
  val formatDecimals: Int)(
  implicit context: MoneyContextLike[C], ops: TerraOps[C])
    extends UnitOfMeasure[MoneyLike[C], C#TC, C] {

  type Money = MoneyLike[C]
  type MoneyContext = MoneyContextLike[C]

  override def getTag(implicit ops: TerraOps[C]): PseudoClassTag[C#TC] =
    ops.getClassTagTC

  def apply(d: C#TC)(implicit ops: TerraOps[C]): Money = new Money(d, this)
  override def apply[A](a: A)(implicit n: Numeric[A], ops: TerraOps[C]) = {
    new Money(ops.convCurrency(n.toDouble(a)), this)
  }

  protected def converterFrom(implicit ops: TerraOps[C]): C#TC ⇒ C#TC = ???
    //context.convert(_, this)
  protected def converterTo(implicit ops: TerraOps[C]): C#TC ⇒ C#TC = ???
    //context.convert(_, this)

  def /(that: Money)(implicit ops: TerraOps[C]): CurrencyExchangeRate[C] =
    that toThe new Money(ops.numC.one, this)

  override def makeEnsureType(implicit ops: TerraOps[C]): HasEnsureType[C#TC] =
    ops.converters.ensureTC

  override private[terra] def makeDoubleConverter(
    implicit ops: TerraOps[C]): HasConverter[Double, C#TC] =
    ops.converters.dtcConverter.asInstanceOf[HasConverter[Double, C#TC]]
  override private[terra] def makeLongConverter(
    implicit ops: TerraOps[C]): HasConverter[Long, C#TC] = 
    ops.converters.ltcConverter.asInstanceOf[HasConverter[Long, C#TC]]

  override def toString: String = code
  override def equals(o: Any): Boolean = o match {
    case c: CurrencyLike[C] => c.code == code
    case pc: ops.moneyOps.PseudoCurrency => pc.code == code //equals(pc.convert)
    case _ => false
  }
  override def hashCode: Int = code.hashCode
}

/**
  * Factory singleton for Money
  */
class MoneyDim[C <: TypeContext](
  implicit context: MoneyContextLike[C], ops: TerraOps[C])
    extends Dimension[MoneyLike[C], C#TC, C] {

  type Money = MoneyLike[C]
  type Currency = CurrencyLike[C]

  def apply[N](a: N)(implicit n: Numeric[N]) =
    new Money(ops.convCurrency(n.toDouble(a)), context.defaultCurrency)
  def apply(value: C#TC) = new Money(value, context.defaultCurrency)

  def apply(value: C#TC, currency: Currency) = new Money(value, currency)
  def apply(value: C#TC, currency: String) =
    new Money(value, context.currency(currency))

  def apply[A](a: A, currency: Currency)(implicit n: Numeric[A]) =
    new Money(ops.convCurrency(BigDecimal(n.toDouble(a))), currency)
  def apply[A](a: A, currency: String)(implicit n: Numeric[A]) =
    new Money(
      ops.convCurrency(BigDecimal(n.toDouble(a))),
      context.currency(currency))

  def apply(s: String): Try[Money] = {
    lazy val regex = ("([-+]?[0-9]*\\.?[0-9]+) *(" + context.currencies.map(_.code).reduceLeft(_ + "|" + _) + ")").r
    s match {
      case regex(value, currency) ⇒ Success(context.dimension(value.toDouble, context.currency(currency)))
      case _                      ⇒ Failure(QuantityParseException("Unable to parse Money", s))
    }
  }

  def name = "Money"

  def parseString(s: String): Try[MoneyLike[C]] = apply(s)

  def primaryUnit = ??? // Should not be used with Money - drawn from MoneyContext instead
  def siUnit = ??? // Should not be used with Money - drawn from MoneyContext instead
  def units = ??? // Should not be used with Money - drawn from MoneyContext instead
}

trait MoneyOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]

  type Money = MoneyLike[C]
  type MoneyContext = MoneyContextLike[C]
  type Currency = CurrencyLike[C]

  // primordial money context to break circular dependency between MoneyContexts
  // and currencies, its only used to make an initial set of Currencies below
  // and then never used again.  That's why all of its methods are stubbed out.
  private[this] val primordial = new MoneyContextLike[C]() {
    override def currency(code: String): Currency = ???
    override def directRateFor(
      curA: Currency, curB: Currency): Option[CurrencyExchangeRate[C]] = ???
    override def indirectRateFor(curA: Currency, curB: Currency)(
      implicit ops: TerraOps[C]): Option[CurrencyExchangeRate[C]] = ???
    override def convert(money: Money, currency: Currency)(
      implicit ops: TerraOps[C]): Money = ???
    override def withAdditionalCurrencies(additionalCurrencies: Set[Currency]) =
      ???
    override def withExchangeRates(rates: List[CurrencyExchangeRate[C]]) = ???
  }

  lazy val defaultMoneyContext = makeDefaultContext
  private[this] def makeDefaultContext: MoneyContext = {

    val usd = primordial.currency("USD", "US Dollar", "$", 2)
    val defaultCurrencySet = Set(
      usd,
      primordial.currency("ARS", "Argentinean Peso", "$", 2),
      primordial.currency("AUD", "Australian Dollar", "$", 2),
      primordial.currency("BRL", "Brazilian Real", "R$", 2),
      primordial.currency("CAD", "Canadian Dollar", "$", 2),
      primordial.currency("CHF", "Swiss Franc", "CHF", 2),
      primordial.currency("CLP", "Chilean Peso", "¥", 2),
      primordial.currency("CNY", "Chinese Yuan Renmimbi", "¥", 2),
      primordial.currency("CZK", "Czech Republic Koruny", "Kč", 2),
      primordial.currency("DKK", "Danish Kroner", "kr", 2),
      primordial.currency("EUR", "Euro", "€", 2),
      primordial.currency("GBP", "British Pound", "£", 2),
      primordial.currency("HKD", "Hong Kong Dollar", "$", 2),
      primordial.currency("INR", "Indian Rupee", "₹", 2),
      primordial.currency("JPY", "Japanese Yen", "¥", 0),
      primordial.currency("KRW", "South Korean Won", "kr", 0),
      primordial.currency("MXN", "Mexican Peso", "$", 2),
      primordial.currency("MYR", "Malaysian Ringgit", "RM", 2),
      primordial.currency("NOK", "Norwegian Krone", "kr", 2),
      primordial.currency("NZD", "New Zealand Dollar", "$", 2),
      primordial.currency("RUB", "Russian Ruble", "руб", 2),
      primordial.currency("SEK", "Swedish Kroner", "kr", 2),
      primordial.currency("XAG", "Silver", "oz", 4),
      primordial.currency("XAU", "Gold", "oz", 4),
      primordial.currency("BTC", "BitCoin", "B", 15),
      primordial.currency("ETH", "Ether", "\u039e", 15),
      primordial.currency("LTC", "Litecoin", "\u0141", 15),
      primordial.currency("ZAR", "South African Rand", "R", 2),
      primordial.currency("NAD", "Namibian Dollar", "N$", 2))

    val defaultCurrencyMap: Map[String, Currency] =
      defaultCurrencySet.map { c: Currency ⇒ c.code -> c }.toMap

    // when the currencies and rates are passed to the MoneyContext's 
    // constructor, they are cloned into versions that use the MoneyContext
    // itself as their MoneyContext
    new MoneyContextLike[C](Some(usd), defaultCurrencySet, Nil)
  }

  lazy val USD = defaultMoneyContext.currency("USD")
  lazy val ARS = defaultMoneyContext.currency("ARS")
  lazy val AUD = defaultMoneyContext.currency("AUD")
  lazy val BRL = defaultMoneyContext.currency("BRL")
  lazy val CAD = defaultMoneyContext.currency("CAD")
  lazy val CHF = defaultMoneyContext.currency("CHF")
  lazy val CLP = defaultMoneyContext.currency("CLP")
  lazy val CNY = defaultMoneyContext.currency("CNY")
  lazy val CZK = defaultMoneyContext.currency("CZK")
  lazy val DKK = defaultMoneyContext.currency("DKK")
  lazy val EUR = defaultMoneyContext.currency("EUR")
  lazy val GBP = defaultMoneyContext.currency("GBP")
  lazy val HKD = defaultMoneyContext.currency("HKD")
  lazy val INR = defaultMoneyContext.currency("INR")
  lazy val JPY = defaultMoneyContext.currency("JPY")
  lazy val KRW = defaultMoneyContext.currency("KRW")
  lazy val MXN = defaultMoneyContext.currency("MXN")
  lazy val MYR = defaultMoneyContext.currency("MYR")
  lazy val NOK = defaultMoneyContext.currency("NOK")
  lazy val NZD = defaultMoneyContext.currency("NZD")
  lazy val RUB = defaultMoneyContext.currency("RUB")
  lazy val SEK = defaultMoneyContext.currency("SEK")
  lazy val XAG = defaultMoneyContext.currency("XAG")
  lazy val XAU = defaultMoneyContext.currency("XAU")
  lazy val BTC = defaultMoneyContext.currency("BTC")
  lazy val ETH = defaultMoneyContext.currency("ETH")
  lazy val LTC = defaultMoneyContext.currency("LTC")
  lazy val ZAR = defaultMoneyContext.currency("ZAR")
  lazy val NAD = defaultMoneyContext.currency("NAD")

  /**
    * Support for Money DSL
    */
  object MoneyConversions {
    def dollar(implicit context: MoneyContext) = context.dimension(1, USD)
    def euro(implicit context: MoneyContext) = context.dimension(1, EUR)
    def yen(implicit context: MoneyContext) = context.dimension(1, JPY)

    implicit def fromLong(l: Long): MoneyConversions[C#TC] = {
      implicit val n: Numeric[C#TC] = ops.numC
      new MoneyConversions(ops.convCurrency(l))
    }
    implicit def fromDouble(d: Double): MoneyConversions[C#TC] = {
      implicit val n: Numeric[C#TC] = ops.numC
      new MoneyConversions(ops.convCurrency(d))
    }

    implicit class MoneyConversions[A](a: A)(implicit n: Numeric[A]) {
      def money(implicit context: MoneyContext) = 
        context.dimension(a, context.defaultCurrency)
      def XAU(implicit context: MoneyContext) = 
        context.dimension(a, context.currency("XAU"))
      def XAG(implicit context: MoneyContext) =
        context.dimension(a, context.currency("XAG"))
      def USD(implicit context: MoneyContext) = 
        context.dimension(a, context.currency("USD"))
      def dollars(implicit context: MoneyContext) = 
        context.dimension(a, context.currency("USD"))
      def cents(implicit context: MoneyContext) = {
        implicit val e: HasEnsureType[C#TC] = ops.converters.ensureTC
        implicit val tag: PseudoClassTag[C#TC] = ops.getClassTagTC
        implicit val num: Numeric[C#TC] = ops.numC
        context.dimension(
          ops.div[C#TC](
            ops.convCurrency(n.toDouble(a)), ops.convCurrency(100d)),
          context.currency("USD"))
      }
      def EUR(implicit context: MoneyContext) = 
        context.dimension(a, context.currency("EUR"))
      def euros(implicit context: MoneyContext) = 
        context.dimension(a, context.currency("EUR"))
      def JPY(implicit context: MoneyContext) = 
        context.dimension(a, context.currency("JPY"))
      def yen(implicit context: MoneyContext) = 
        context.dimension(a, context.currency("JPY"))
      def GBP(implicit context: MoneyContext) = 
        context.dimension(a, context.currency("GBP"))
      def poundSterling(implicit context: MoneyContext) = 
        context.dimension(a, context.currency("GBP"))
      def CHF(implicit context: MoneyContext) = 
        context.dimension(a, context.currency("CHF"))
      def swissFrancs(implicit context: MoneyContext) = 
        context.dimension(a, context.currency("CHF"))
      def AUD(implicit context: MoneyContext) = 
        context.dimension(a, context.currency("AUD"))
      def CAD(implicit context: MoneyContext) = 
        context.dimension(a, context.currency("CAD"))
      def SEK(implicit context: MoneyContext) = 
        context.dimension(a, context.currency("SEK"))
      def HKD(implicit context: MoneyContext) = 
        context.dimension(a, context.currency("HKD"))
      def NOK(implicit context: MoneyContext) = 
        context.dimension(a, context.currency("NOK"))
      def NZD(implicit context: MoneyContext) = 
        context.dimension(a, context.currency("NZD"))
      def BTC(implicit context: MoneyContext) = 
        context.dimension(a, context.currency("BTC"))
      def bitCoin(implicit context: MoneyContext) = 
        context.dimension(a, context.currency("BTC"))
      def ETH(implicit context: MoneyContext) = 
        context.dimension(a, context.currency("ETH"))
      def ether(implicit context: MoneyContext) = 
        context.dimension(a, context.currency("ETH"))
      def LTC(implicit context: MoneyContext) = 
        context.dimension(a, context.currency("LTC"))
      def liteCoin(implicit context: MoneyContext) = 
        context.dimension(a, context.currency("LTC"))
      def ZAR(implicit context: MoneyContext) = 
        context.dimension(a, context.currency("ZAR"))
      def NAD(implicit context: MoneyContext) = 
        context.dimension(a, context.currency("NAD"))

      def times(that: Money)(
        implicit context: MoneyContext = defaultMoneyContext): Money =
        that.times(n.toDouble(a))
      def *(that: Money)(
        implicit context: MoneyContext = defaultMoneyContext): Money = 
        times(that)
    }
  }

  case class PseudoMoney1(value: C#TC) {
    def convert(implicit context: MoneyContext = defaultMoneyContext): Money =
      new Money(value, context.defaultCurrency)
    lazy val del = convert
    override def toString: String = del.toString
    override def equals(o: Any): Boolean = o match {
      case pm: PseudoMoney1 => pm.del.equals(del)
      case pm: PseudoMoney2 => pm.del.equals(del)
      case pm: PseudoMoneyStr => pm.del.equals(del)
      case m: Money => del.equals(m)
      case _ => false
    }
    override def hashCode: Int = del.hashCode
  }

  case class PseudoMoney2(value: C#TC, currency: Currency) {
    def convert(implicit context: MoneyContext = defaultMoneyContext): Money =
      new Money(value, currency)
    lazy val del = convert
    override def toString: String = del.toString
    override def equals(o: Any): Boolean = {
      o match {
        case pm: PseudoMoney1 => pm.del.equals(del)
        case pm: PseudoMoney2 => pm.del.equals(del)
        case pm: PseudoMoneyStr => pm.del.equals(del)
        case m: Money => del.equals(m)
        case _ => false
      }}
    override def hashCode: Int = del.hashCode
  }

  case class PseudoMoneyStr(s: String) {
    def convert(
      implicit context: MoneyContext = defaultMoneyContext): Try[Money] =
      context.dimension(s)
    lazy val del = convert.get
    override def toString: String = del.toString
    override def equals(o: Any): Boolean = o match {
      case pm: PseudoMoney1 => pm.del.equals(del)
      case pm: PseudoMoney2 => pm.del.equals(del)
      case pm: PseudoMoneyStr => pm.del.equals(del)
      case m: Money => del.equals(m)
      case _ => false
    }
    override def hashCode: Int = del.hashCode
  }

  case class PseudoCurrency(code: String) {
    def convert(
      implicit context: MoneyContext = defaultMoneyContext): Currency =
      context.currency(code)
    lazy val del = convert //pseudoCurrencyToCurrency(this)
    override def toString: String = del.toString
    override def equals(o: Any): Boolean =
      o match {
        case pc: PseudoCurrency => pc.del.equals(del)
        case c: Currency => c.equals(del)
        case _ => false
      }
    override def hashCode: Int = del.hashCode
  }
}
