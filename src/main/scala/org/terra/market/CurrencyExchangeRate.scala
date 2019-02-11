/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package market

import scala.reflect.ClassTag

/**
 * Represent the rate of exchange between two currencies
 *
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param base the base or "Fixed" currency (usually a value of 1 currency unit, but not required)
 * @param counter the counter or "Variable" currency
 */
case class CurrencyExchangeRate[C <: TypeContext](
  base: MoneyLike[C], counter: MoneyLike[C])(
  implicit ops: TerraOps[C])
    extends Ratio[MoneyLike[C], MoneyLike[C], C#TC, C] {
  require(base.currency != counter.currency, "Can not create Exchange Rate on matching currencies")

  type Money = MoneyLike[C]

  /**
    * @return Double the rate = counter / base
    */
  def rate: C#TC = {
    implicit val e: HasEnsureType[C#TC] = ops.converters.ensureTC
    implicit val tag: ClassTag[C#TC] = ops.getClassTagTC
    ops.div[C#TC](counter.value, base.value)
  }

  /**
    * Converts the given money into the other currency of this exchange rate
    *
    * @param money Money
    * @return
    */
  def convert(money: Money) = money.currency match {
    case base.currency    ⇒ convertToCounter(money)
    case counter.currency ⇒ convertToBase(money)
    case _                ⇒ throw new IllegalArgumentException("The currency of money must match the currency of base or counter")
  }

  /** convert  */
  def *(money: Money) = convert(money)

  /**
    * Override methods from Ratio to ensure BigDecimal precision math is applied
    *
    * @param m Money
    * @return
    */
  override def convertToBase(m: Money)(implicit ops: TerraOps[C]): Money = 
    base * ops.convCurrency(m / counter)
  override def convertToCounter(m: Money)(implicit ops: TerraOps[C]): Money = 
    counter * ops.convCurrency(m / base)

  /**
   * Returns the rate formatted in as standard FX Quote"
   * @return
   */
  override def toString = {
    implicit val n: Numeric[C#TC] = ops.numC
    s"${base.currency.code}/${counter.currency.code} ${crossFormat(rate)}"
  }
}
