/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package market

/**
 * Represents a price
 *
 * A price is an [[org.terra.Ratio]] between a quantity of [[org.terra.market.Money]]
 * and some other [[org.terra.Quantity]]
 *
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param money Money
 * @param quantity Quantity
 * @tparam A Quantity Type
 */
case class Price[A <: Quantity[A, T, C], T, C <: TypeContext](
  money: MoneyLike[C], quantity: A)(implicit ops: TerraOps[C]) {

  type Money = MoneyLike[C]
  type PriceT = Price[A, T, C]
  type Currency = CurrencyLike[C]

  def base: MoneyLike[C] = money
  def counter: A = quantity
  def convertToBase(q: A)(implicit ops: TerraOps[C]): MoneyLike[C] = {
    implicit val e: HasConverter[T, C#TC] = quantity.makeToCurrencyConverter
    base * ops.convCurrency(q / counter)
  }
  def convertToCounter(q: MoneyLike[C])(implicit ops: TerraOps[C]): A = {
    implicit val e: HasEnsureType[T] = counter.makeEnsureType
    counter * ops.ensureType[T](q / base)
  }

  // TODO Add verification that money amounts are the same OR convert
  def plus(that: PriceT)(implicit ops: TerraOps[C]): PriceT = 
    new PriceT(this.money + that.money, quantity)
  def +(that: PriceT)(implicit ops: TerraOps[C]): PriceT = plus(that)
  def minus(that: PriceT)(implicit ops: TerraOps[C]): PriceT = 
    new PriceT(this.money - that.money, quantity)
  def -(that: PriceT): PriceT = minus(that)

  //def times(that: Double)(implicit ops: TerraOps[C]): PriceT =
    //new PriceT(this.money * ops.convCurrency(that), quantity)
  // TODO Double???
  //def *(that: Double)(implicit ops: TerraOps[C]): PriceT =
    //new PriceT(this.money * ops.convCurrency(that), quantity)
  def times(that: C#TC)(implicit ops: TerraOps[C]): PriceT =
    new PriceT(this.money * that, quantity)
  def *(that: C#TC)(implicit ops: TerraOps[C]): PriceT = 
    new PriceT(this.money * that, quantity)

  def divide(that: Double)(implicit ops: TerraOps[C]): PriceT = 
    new PriceT(this.money / ops.convCurrency(that), quantity)
  def /(that: Double)(implicit ops: TerraOps[C]): PriceT = divide(that)
  def divide(that: C#TC)(implicit ops: TerraOps[C]): PriceT = 
    new PriceT(this.money / that, quantity)
  def /(that: C#TC)(implicit ops: TerraOps[C]): PriceT = divide(that)
  def divide(that: PriceT)(implicit ops: TerraOps[C]): C#TC = {
    implicit val e: HasEnsureType[C#TC] = ops.converters.ensureTC
    implicit val tag: PseudoClassTag[C#TC] = ops.getClassTagTC
    ops.div[C#TC](this.money.value, that.money.value)
  }
  def /(that: PriceT)(implicit ops: TerraOps[C]): C#TC = divide(that)

  def in(currency: Currency)(
    implicit moneyContext: MoneyContextLike[C], ops: TerraOps[C]) =
    new PriceT((money in currency), quantity)

  /**
   * Returns the Cost (Money) for a quantity `that` of A
   * @param that Quantity
   * @return
   */
  def *(that: A): Money = convertToBase(that)

  /**
   * Returns the Quantity that will cost that)
   * @param that Money
   * @return
   */
  @deprecated("Use `money / price` instead", "0.6.3")
  def *(that: Money): A = that / this

  override def toString = money.toString + "/" + quantity.toString

  def toString(unit: UnitOfMeasure[A, T, C]) = 
    money.toString + "/" + quantity.toString(unit)

  def toString(currency: Currency, unit: UnitOfMeasure[A, T, C])(
    implicit moneyContext: MoneyContextLike[C]) =
    (money in currency).toString + "/" + quantity.toString(unit)
}

