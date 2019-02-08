
package org.terra
package market

import scala.util.Try

/**
  * Contains all the symbols exported by the org.terra.*.market packages.
  * Synthetic packages then subclass this trait with an object to represent
  * a package in the synthetic trees of symbols used by the unit test and
  * user's calling code.
  * <p>
  * This symbols trait is a bit special as the market package's use of an
  * implicit variable called MoneyContext introduces difficulties in making
  * the objects in this package work as desired by the original squants DSL.
  * For instance, the MoneyContext and Currency objects make the new code look
  * like the old api.  Additionally, there are Pseudo* classes in MoneyOps
  * that use type conversions here to inject the implicit MoneyContext at
  * the place in the code where it is used instead of when the market package
  * is loaded.  This allows use to use value values for the money context
  * while avoiding forcing use of the default contexts in surprising places
  * like the old Squants DSL did.
  */
trait MarketSymbols[Tuple <: TypeContext] {

  implicit val ops: TerraOps[Tuple]

  type Money = MoneyLike[Tuple]
  type PricePer[A <: Quantity[A, Tuple#T, Tuple]] = Price[A, Tuple#T, Tuple]
  type PricePerL[A <: Quantity[A, Tuple#TL, Tuple]] = Price[A, Tuple#TL, Tuple]
  type PricePerT[A <: Quantity[A, Tuple#TT, Tuple]] = Price[A, Tuple#TT, Tuple]
  type MoneyContext = MoneyContextLike[Tuple]
  type Currency = CurrencyLike[Tuple]
  lazy val defaultMoneyContext = ops.moneyOps.defaultMoneyContext

  object MoneyContext {
    def apply(
      defaultCurrency: Currency,
      currencies: Set[Currency],
      rates: Seq[CurrencyExchangeRate[Tuple]],
      allowIndirectConversions: Boolean = true)(
      implicit ops: TerraOps[Tuple]): MoneyContext =
      new MoneyContext(
        Some(defaultCurrency), currencies, rates, allowIndirectConversions)
  }

  object Currency {
    def apply(
      code: String, name: String, symbol: String, formatDecimals: Int)(
      implicit context: MoneyContextLike[Tuple]) =
      new CurrencyLike[Tuple](code, name, symbol, formatDecimals)
  }

  import ops.moneyOps.{ PseudoMoney1, PseudoMoney2, PseudoMoneyStr, PseudoCurrency }

  lazy val USD = PseudoCurrency("USD")
  lazy val ARS = PseudoCurrency("ARS")
  lazy val AUD = PseudoCurrency("AUD")
  lazy val BRL = PseudoCurrency("BRL")
  lazy val CAD = PseudoCurrency("CAD")
  lazy val CHF = PseudoCurrency("CHF")
  lazy val CLP = PseudoCurrency("CLP")
  lazy val CNY = PseudoCurrency("CNY")
  lazy val CZK = PseudoCurrency("CZK")
  lazy val DKK = PseudoCurrency("DKK")
  lazy val EUR = PseudoCurrency("EUR")
  lazy val GBP = PseudoCurrency("GBP")
  lazy val HKD = PseudoCurrency("HKD")
  lazy val INR = PseudoCurrency("INR")
  lazy val JPY = PseudoCurrency("JPY")
  lazy val KRW = PseudoCurrency("KRW")
  lazy val MXN = PseudoCurrency("MXN")
  lazy val MYR = PseudoCurrency("MYR")
  lazy val NOK = PseudoCurrency("NOK")
  lazy val NZD = PseudoCurrency("NZD")
  lazy val RUB = PseudoCurrency("RUB")
  lazy val SEK = PseudoCurrency("SEK")
  lazy val XAG = PseudoCurrency("XAG")
  lazy val XAU = PseudoCurrency("XAU")
  lazy val BTC = PseudoCurrency("BTC")
  lazy val ETH = PseudoCurrency("ETH")
  lazy val LTC = PseudoCurrency("LTC")
  lazy val ZAR = PseudoCurrency("ZAR")
  lazy val NAD = PseudoCurrency("NAD")

  implicit val moneyNumeric: Numeric[Money] = defaultMoneyContext.numeric

  object MoneyConversions {
    import ops.moneyOps.{ MoneyConversions => Convs }

    def dollar(implicit context: MoneyContext) = Convs.dollar
    def euro(implicit context: MoneyContext) = Convs.euro
    def yen(implicit context: MoneyContext) = Convs.yen

    implicit def fromLong(l: Long): Convs.MoneyConversions[Tuple#TC] =
      Convs.fromLong(l)
    implicit def fromLong2(l: Long): Tuple#TC = {
      implicit val e: HasConverter[Long, Tuple#TC] =
        ops.converters.ltcConverter
      ops.gconvTotal[Long, Tuple#TC](l)
      }
    implicit def fromDouble(d: Double): Convs.MoneyConversions[Tuple#TC] =
      Convs.fromDouble(d)
    implicit def fromDouble2(d: Double): Tuple#TC = {
      implicit val e: HasConverter[Double, Tuple#TC] =
        ops.converters.dtcConverter
      ops.gconvTotal[Double, Tuple#TC](d)
    }

    implicit class MoneyConversions[A](a: A)(implicit num: Numeric[A])
        extends Convs.MoneyConversions[A](a)
  }

  implicit def stringToCurrency(s: String)(implicit context: MoneyContext) =
    context.currency(s)

  // implicit conversions that happen in the presence of the implicit 
  // MoneyContext that turn different combinations of constructor args into
  // the Money or Currency object we will use that now has the correct
  // MoneyContext in it
  implicit def pseudoMoney1ToMoney(pseudo: PseudoMoney1)(
    implicit context: MoneyContext = defaultMoneyContext): Money =
    pseudo.convert
  implicit def pseudoMoney2ToMoney(pseudo: PseudoMoney2)(
    implicit context: MoneyContext = defaultMoneyContext): Money =
    pseudo.convert
  implicit def pseudoMoneyStrToMoney(pseudo: PseudoMoneyStr)(
    implicit context: MoneyContext = defaultMoneyContext): Try[Money] =
    pseudo.convert
  implicit def pseudoCurrencyToCurrency(pseudo: PseudoCurrency)(
    implicit context: MoneyContext = defaultMoneyContext): Currency =
    pseudo.convert //context.currency(pseudo.code)

  // masks the Money constructors to make Pseudo* classes instead that are
  // converted to Money or Currency objects in calling code invisibly upon
  // first use, which is where the implicit MoneyContext is injected
  object Money {
    def apply(t: Tuple#TC): PseudoMoney1 = PseudoMoney1(t)
    def apply[N](a: N)(implicit n: Numeric[N]): PseudoMoney1 =
      PseudoMoney1(ops.convCurrency(n.toDouble(a)))

    def apply(t: Tuple#TC, currency: Currency) =
      PseudoMoney2(t, currency)
    def apply[A](a: A, currency: Currency)(
      implicit n: Numeric[A]): PseudoMoney2 =
      PseudoMoney2(ops.convCurrency(n.toDouble(a)), currency)

    def apply(s: String)(
      implicit context: MoneyContext = defaultMoneyContext): PseudoMoneyStr =
      PseudoMoneyStr(s)

    def parseString(s: String)(
      implicit context: MoneyContext = defaultMoneyContext): PseudoMoneyStr =
      apply(s)
  }
}
