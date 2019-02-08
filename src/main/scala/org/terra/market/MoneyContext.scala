/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package market

import scala.util.{ Success, Failure }

/**
 * MoneyContext
 *
 * Provides a context for Money specific operations.
 *
 * When provided as an implicit parameter, the defaultCurrency will be used by the
 * Money factory when no other currency is provided.
 *
 * Provides for cross-currency conversions.
 *
 * Will act as an implicit parameter to cross currency operations to allow for
 * easy conversions
 *
 * @author  garyKeorkunian
 * @since   0.1
 * @param defaultCurrency Currency used when none is supplied to the Money factory
 * @param rates Collection of Exchange Rates used for currency conversions
 */
case class MoneyContextLike[C <: TypeContext](
  _defaultCurrency: Option[CurrencyLike[C]] = None,
  _currencies: Set[CurrencyLike[C]] = Set[CurrencyLike[C]](),
  _rates: Seq[CurrencyExchangeRate[C]] = Seq[CurrencyExchangeRate[C]](),
  allowIndirectConversions: Boolean = true)(implicit ops: TerraOps[C]) {

  type Money = MoneyLike[C]
  type MoneyDimension = MoneyDim[C]
  type Currency = CurrencyLike[C]

  implicit val ctx = this
  private[this] def clone(currency: Currency): Currency = new Currency(
    currency.code, currency.name, currency.symbol, currency.formatDecimals)

  // replaces the default context with this context in the given classes
  val currencies = _currencies.map { clone(_) }
  val defaultCurrency: Currency = _defaultCurrency match {
    case Some(curr) => currencies.find(_.code == curr.code).getOrElse(
      throw new IllegalArgumentException(
        s"default currency ${curr} isn't in list of currencies ${_currencies}"))
    case None => null
  }
  val rates = _rates.map { rate => CurrencyExchangeRate(
    new Money(rate.base.value, currency(rate.base.currency.code)), 
    new Money(rate.counter.value, currency(rate.counter.currency.code)))
  }

  lazy val dimension: MoneyDimension = new MoneyDim[C]

  def currency(
    code: String, name: String, symbol: String, formatDecimals: Int): 
      Currency = {
    implicit val ctx = this
    assert(currencies.find(_.code == code) == None)
    new Currency(code, name, symbol, formatDecimals)
  }

  /**
    * Custom implementation using SortedSets to ensure consistent output
    * @return String representation of this instance
    */
  override def toString: String = string
  private lazy val string = {
    val cSet = currencies.map(_.toString).toSeq.sorted.mkString(",")
    val rSet = rates.map(_.toString).sorted.mkString(",")
    s"MoneyContext(DefaultCurrency(${defaultCurrency.code}),Currencies($cSet),ExchangeRates($rSet),AllowIndirectConversions($allowIndirectConversions))"
  }

  def currency(code: String): Currency = 
    currencies.find(code == _.code).getOrElse(
      throw new IllegalArgumentException(s"bad currency code $code"))

  /**
   * Returns an Option on an exchange rate if a direct rate exists, otherwise None
   *
   * @param curA Currency A
   * @param curB Currency B
   * @return
   */
  def directRateFor(curA: Currency, curB: Currency): Option[CurrencyExchangeRate[C]] = {
    if (rates.isEmpty) 
      throw new UnsupportedOperationException(
        "no exchange rates present, you probably need to declare an implicit MoneyContext")
    rates.find(r ⇒
      r.base.currency.code == curA.code && 
        r.counter.currency.code == curB.code ||
        r.base.currency.code == curB.code && 
        r.counter.currency.code == curA.code)
  }

  /**
   * Return an Option on an exchange rate.  If a direct rate exists an Option on that will be returned.
   * Otherwise, if a cross rate can be determined (1 hop limit), it will be created and returned in an Option.
   * Otherwise, None will be returned
   *
   * @param curA Currency A
   * @param curB Currency B
   * @return
   */
  def indirectRateFor(curA: Currency, curB: Currency)(
    implicit ops: TerraOps[C]): Option[CurrencyExchangeRate[C]] = {

    // TODO Improve this to attempt to use defaultCurrency first
    directRateFor(curA, curB) match {
      case Some(rate) ⇒ Some(rate)
      case _ ⇒
        val ratesWithCurA = rates.filter(r ⇒ r.base.currency == curA || r.counter.currency == curA)
        val ratesWithCurB = rates.filter(r ⇒ r.base.currency == curB || r.counter.currency == curB)

        val curs = for {
          cur ← currencies
          if ratesWithCurA.map(_.base.currency).contains(cur) || 
          ratesWithCurA.map(_.counter.currency).contains(cur)
          if ratesWithCurB.map(_.base.currency).contains(cur) || ratesWithCurB.map(_.counter.currency).contains(cur)
        } yield cur

        curs.headOption match {
          case Some(cur) ⇒ Some(CurrencyExchangeRate(convert(cur(1), curA), convert(cur(1), curB)))
          case None      ⇒ None
        }
    }
  }

  /**
   * Converts a Money value to the specified currency.
   *
   * The conversion first attempts to use an existing exchange rate for the two currencies in question.
   * If no direct exchange works, a cross rate (limited to 1 hop) will be calculated and used.
   * If no cross rate can be calculated a NoSuchElementException is thrown
   *
   * @param money  Money to be converted
   * @param currency Currency to be converted to
   * @return
   * @throws NoSuchExchangeRateException when no exchange rate is available
   */
  def convert(money: Money, currency: Currency)(
    implicit ops: TerraOps[C]): Money = {

    if (money.currency == currency) money
    else directRateFor(money.currency, currency) match {
      case Some(rate) ⇒ rate.convert(money)
      case _ if allowIndirectConversions ⇒ {
        indirectRateFor(money.currency, currency) match {
          case Some(crossRate) ⇒ crossRate.convert(money)
          case None            ⇒ throw new NoSuchExchangeRateException(s"Rate for currency pair (${money.currency} / $currency)")
        }
      }
      case _ ⇒ throw new NoSuchExchangeRateException(s"Rate for currency pair (${money.currency} / $currency)")
    }
  }

  def numeric: Numeric[MoneyLike[C]] = MoneyNumeric

  object MoneyNumeric extends Numeric[MoneyLike[C]] {
    def plus(x: Money, y: Money) = x + y
    def minus(x: Money, y: Money) = x - y
    def times(x: Money, y: Money) = throw new UnsupportedOperationException("Numeric.times not supported for Quantities")
    def negate(x: Money) = -x
    def fromInt(x: Int) = defaultCurrency(x)
    def toInt(x: Money) = ops.numC.toInt(x.value)
    def toLong(x: Money) = ops.numC.toLong(x.value)
    def toFloat(x: Money) = ops.numC.toFloat(x.value)
    def toDouble(x: Money) = ops.numC.toDouble(x.value)
    def compare(x: Money, y: Money) = if (ops.numC.gt(x.value, y.value)) 1 else if (ops.numC.lt(x.value, y.value)) -1 else 0

    def parseString(str: String): Option[Money] = dimension(str) match {
      case Success(a) => Some(a)
      case Failure(_) => None
    }

    /**
      * Custom implementation using SortedSets to ensure consistent output
      * @return String representation of this instance
      */
    override def toString: String = s"MoneyNumeric"
  }

  /**
   * Adds two money values that may or may not be in the same currency.
   *
   * The result will be in the same currency as the first parameter.
   *
   * @param moneyA Money A
   * @param moneyB Money B
   * @return
   * @throws NoSuchExchangeRateException when no exchange rate is available
   */
  //def add(moneyA: Money, moneyB: Money): Money = moneyA.currency(moneyA.amount + convert(moneyB, moneyA.currency).amount)

  /**
   * Subtracts two money values that may or may not be in the same currency
   *
   * The result will be in the same currency as the first parameter.
   *
   * @param moneyA Money A
   * @param moneyB Money B
   * @return
   * @throws NoSuchExchangeRateException when no exchange rate is available
   */
  //def subtract(moneyA: Money, moneyB: Money): Money = moneyA.currency(moneyA.amount - convert(moneyB, moneyA.currency).amount)

  /**
   * Divides two money value that may or may not be in the same currency after converting the second to the first
   *
   * @param moneyA Money A
   * @param moneyB Money B
   * @return
   */
  //def divide(moneyA: Money, moneyB: Money): BigDecimal = moneyA.amount / convert(moneyB, moneyA.currency).amount

  /**
   * Performs a standard compare on two money values that may or may not be in the same currency
   * @param moneyA Money A
   * @param moneyB Money B
   * @return
   * @throws NoSuchExchangeRateException when no exchange rate is available
   */
  //def compare(moneyA: Money, moneyB: Money): Int =
    //if (moneyA.amount > convert(moneyB, moneyA.currency).amount) 1
    //else if (moneyA.amount < convert(moneyB, moneyA.currency).amount) -1
    //else 0

  /**
    * Create a copy of this context with additional currencies added to the existing set
    * @param additionalCurrencies Set[Currency]
    * @return
    */
  def withAdditionalCurrencies(additionalCurrencies: Set[Currency]) = 
    copy(_currencies = currencies ++ additionalCurrencies)

  /**
    * Create a copy of this context with a new list of rates
    * @param rates List[CurrencyExchangeRate]
    * @return
    */
  def withExchangeRates(rates: List[CurrencyExchangeRate[C]]) = 
    copy(_rates = rates)
}

