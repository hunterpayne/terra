# An Extensible Dimensional Analysis Library

Terra is a framework of data types and a domain specific language (DSL) for 
representing Quantities, their Units of Measure, and their Dimensional 
relationships.  The API supports typesafe dimensional analysis, improved 
domain models and more.  All types are immutable and thread-safe.
This library is a rewrite of Squants but fixes bug, adds additional quantities
and adds the ability to swap out the types used by the library to do its
calculations.  Scala 2.11, 2.12 and 2.13 are supported.

## How to use Terra 

[//]: # ( To use Terra in your Maven project, add the following dependency: )
[//]: # ( ```xml )
[//]: # ( <dependency> )
[//]: # (   <groupId>org.kletz</groupId> )
[//]: # (   <artifactId>terra_2.11</artifactId> )
[//]: # (   <version>1.0.0</version> )
[//]: # ( </dependency> )
[//]: # ( ``` )

For the impatient, use these imports:
```scala
import org.terra.standard._
import time.Seconds
import space.Meters
// etc...
```

There are four versions of the Quantity classes to use and you choose which
one you use by importing either:
* import org.terra.standard._ or
* import org.terra.common._ or
* import org.terra.classic._ or
* import org.terra.double._

Then you import the rest of your normal Quantity, Dimension and Unit classes
with all the same overrides, numerics and operator overloads.

Then you can write the exact same code as with Squants (with a few exceptions).
Several bugs from Squants have been fixed including issues with the 
default MoneyContext being used in unexpected places even when a custom
MoneyContext is in scope.  These changes cause the Price type to be changed
to a PricePer type which takes a type parameter which is the type of the
thing being priced.  So price per square yard would be `PricePer[Area]`.


## Type Safe Dimensional Analysis
*The Trouble with Doubles*

When building programs that perform dimensional analysis, developers are quick to declare
quantities using a basic numeric type, usually Double.  While this may be satisfactory in some situations,
it can often lead to semantic and other logic issues.

For example, when using a Double to describe quantities of Energy (kWh) and Power (kW), it is possible
to compile a program that adds these two values together.  This is not appropriate as kW and kWh
measure quantities of two different dimensions.  The unit kWh is used to measure an amount of Energy used
or produced.  The unit kW is used to measure Power/Load, the rate at which Energy is being used
or produced, that is, Power is the first time derivative of Energy.

*Power = Energy / Time*

Consider the following code:

```scala
scala> val loadKw = 1.2
loadKw: Double = 1.2

scala> val energyMwh = 24.2
energyMwh: Double = 24.2

scala> val sumKw = loadKw + energyMwh
sumKw: Double = 25.4
```

This example not only adds quantities of different dimensions (Power vs Energy),
it also fails to convert the scales implied in the val names (Mega vs Kilo).
Because this code compiles, detection of these errors is pushed further into the development cycle.

### Dimensional Type Safety

_Only quantities with the same dimensions may be compared, equated, added, or subtracted._

Terra helps prevent errors like these by type checking operations at compile time and
automatically applying scale and type conversions at run-time.  For example:

```scala
scala> import org.terra.standard.energy.{Kilowatts, Megawatts, Power}
import org.terra.standard.energy.{Kilowatts, Megawatts, Power}

scala> val load1: Power = Kilowatts(12)
load1: org.terra.standard.energy.Power = 12.0 kW

scala> val load2: Power = Megawatts(0.023)
load2: org.terra.standard.energy.Power = 0.023 MW

scala> val sum = load1 + load2
sum: org.terra.standard.energy.Power = 35.0 kW

scala> sum == Kilowatts(35)
res0: Boolean = true

scala> sum == Megawatts(0.035) // comparisions automatically convert scale
res1: Boolean = true
```

The above sample works because Kilowatts and Megawatts are both units of Power.  Only the scale is
different and the library applies an appropriate conversion.  Also, notice that keeping track of
the scale within the value name is no longer needed:

```scala
scala> import org.terra.standard.energy.{Energy, Power, Kilowatts, KilowattHours}
import org.terra.standard.energy.{Energy, Power, Kilowatts, KilowattHours}

scala> val load: Power = Kilowatts(1.2)
load: org.terra.standard.energy.Power = 1.2 kW

scala> val energy: Energy = KilowattHours(23.0)
energy: org.terra.standard.energy.Energy = 23.0 kWh
```

Invalid operations, like adding power and energy, no longer compile:
```scala
scala> val sum = load + energy
<console>:16: error: type mismatch;
 found   : org.terra.standard.energy.Energy
 required: org.terra.standard.energy.Power
       val sum = load + energy
                        ^
```
By using stronger types, we catch the error earlier in the development cycle, preventing the error made when using Double in the example above.

### Dimensionally Correct Type Conversions

_One may take quantities with different dimensions, and multiply or divide them._

Dimensionally correct type conversions are a key feature of Terra.
Conversions are implemented by defining relationships between Quantity types using the `*` and `/` operators.

Code samples in this section assume these imports:
```scala
import org.terra.standard._
import energy.{Kilowatts, Power}
import time.{Hours, Days}
```

The following code demonstrates creating ratio between two quantities of the same dimension,
resulting in a dimensionless value:

```scala
scala> val ratio = Days(1) / Hours(3)
ratio: Double = 8.0
```

This code demonstrates use of the `Power.*` method that takes a `Time` and returns an `Energy`:

```scala
scala> val load = Kilowatts(1.2)
load: org.terra.standard.energy.Power = 1.2 kW

scala> val time = Hours(2)
time: org.terra.standard.time.Time = 2.0 h

scala> val energyUsed = load * time
energyUsed: org.terra.standard.energy.Energy = 2400.0 Wh
```

This code demonstrates use of the `Energy./` method that takes a `Time` and returns a `Power`:

```scala
scala> val aveLoad: Power = energyUsed / time
aveLoad: org.terra.standard.energy.Power = 1200.0 W
```

### Unit Conversions

Code samples in this section assume these imports:
```scala
import scala.language.postfixOps

import org.terra.standard._ // determines which datatypes are used by Terra
import energy.{Gigawatts, Kilowatts, Power, Megawatts}
import mass.MassConversions._
import mass.{Kilograms, Pounds}
import thermal.TemperatureConversions._
import thermal.Fahrenheit
```

Quantity values are based in the units used to create them.

```scala
scala> val loadA: Power = Kilowatts(1200)
loadA: org.terra.standard.energy.Power = 1200.0 kW

scala> val loadB: Power = Megawatts(1200)
loadB: org.terra.standard.energy.Power = 1200.0 MW
```

Since Terra properly equates values of a like dimension, regardless of the unit,
there is usually no reason to explicitly convert from one to the other.
This is especially true if the user code is primarily performing dimensional analysis.

However, there are times when you may need to set a Quantity value to a specific unit (eg, for proper JSON encoding).

When necessary, a quantity can be converted to another unit using the `in` method.

```scala
scala> val loadA = Kilowatts(1200)
loadA: org.terra.standard.energy.Power = 1200.0 kW

scala> val loadB = loadA in Megawatts
loadB: org.terra.standard.energy.Power = 1.2 MW

scala> val loadC = loadA in Gigawatts
loadC: org.terra.standard.energy.Power = 0.0012 GW
```

Sometimes you need to get the numeric value of the quantity in a specific unit
(eg, for submission to an external service that requires a numeric in a specified unit
or to perform analysis beyond Squant's domain)

When necessary, the value can be extracted in the desired unit with the `to` method.

```scala
scala> val load: Power = Kilowatts(1200)
load: org.terra.standard.energy.Power = 1200.0 kW

scala> val kw: Double = load to Kilowatts
kw: Double = 1200.0

scala> val mw: Double = load to Megawatts
mw: Double = 1.2

scala> val gw: Double = load to Gigawatts
gw: Double = 0.0012
```

Most types include methods with convenient aliases for the `to` methods.

```scala
scala> val kw: Double = load toKilowatts
kw: Double = 1200.0

scala> val mw: Double = load toMegawatts
mw: Double = 1.2

scala> val gw: Double = load toGigawatts
gw: Double = 0.0012
```

NOTE - It is important to use the `to` method for extracting the numeric value,
as this ensures you will be getting the numeric value for the desired unit.
`Quantity.value` should not be accessed directly.
To prevent improper usage, direct access to the `Quantity.value` field may be deprecated in a future version.

Creating strings formatted in the desired unit:

```scala
scala> val kw: String = load toString Kilowatts
kw: String = 1200.0 kW

scala> val mw: String = load toString Megawatts
mw: String = 1.2 MW

scala> val gw: String = load toString Gigawatts
gw: String = 0.0012 GW
```

Creating `Tuple2[Double, String]` that includes a numeric value and unit symbol:

```scala
scala> val load: Power = Kilowatts(1200)
load: org.terra.standard.energy.Power = 1200.0 kW

scala> val kw = load toTuple
kw: (Double, String) = (1200.0,kW)

scala> val mw = load toTuple Megawatts
mw: (Double, String) = (1.2,MW)

scala> val gw = load toTuple Gigawatts
gw: (Double, String) = (0.0012,GW)
```

This can be useful for passing properly scaled quantities to other processes
that do not use Terra, or require use of more basic types (Double, String)

Simple console based conversions (using DSL described below)

```scala
scala> 1.kilograms to Pounds
res0: Double = 2.2046226218487757

scala> kilogram / pound
res1: Double = 2.2046226218487757

scala> 2.1.pounds to Kilograms
res2: Double = 0.952543977

scala> 2.1.pounds / kilogram
res3: Double = 0.9525439770000002

scala> 100.C to Fahrenheit
res4: Double = 212.0
```

## Market Package
Market Types are similar but not quite the same as other quantities in the 
library.  The primary type, Money, is a Dimensional Quantity, and its Units of 
Measure are Currencies.  However, because the conversion multipliers 
between currency units can not be predefined, many of the behaviors have been 
overridden and augmented to realize correct behavior.

### Money
A Quantity of purchasing power measured in Currency units.
Like other quantities, the Unit of Measures are used to create Money values.

```scala
scala> import org.terra.standard.market.{BTC, JPY, USD, XAU}
import org.terra.standard.market.{BTC, JPY, USD, XAU}

scala> val tenBucks = USD(10)      // Money: 10 USD
tenBucks: org.terra.standard.market.Money = 1E+1 USD

scala> val someYen = JPY(1200)     // Money: 1200 JPY
someYen: org.terra.standard.market.Money = 1.2E+3 JPY

scala> val goldStash = XAU(50)     // Money: 50 XAU
goldStash: org.terra.standard.market.Money = 5E+1 XAU

scala> val digitalCache = BTC(50)  // Money: 50 BTC
digitalCache: org.terra.standard.market.Money = 5E+1 BTC
```

### Price
A Ratio between Money and another Quantity.
A Price value is typed on a Quantity and can be denominated in any defined Currency.

*Price = Money / Quantity*

Assuming these imports:
```scala
import org.terra.standard._
import energy.MegawattHours
import market.USD
import space.UsGallons
```

You can compute the following:
```scala
scala> val threeForADollar = USD(1) / Each(3)
threeForADollar: org.terra.standard.market.Price[org.terra.standard.Dimensionless] = 1 USD/3.0 ea

scala> val energyPrice = USD(102.20) / MegawattHours(1)
energyPrice: org.terra.standard.market.Price[org.terra.standard.energy.Energy] = 102.2 USD/1.0 MWh

scala> val milkPrice = USD(4) / UsGallons(1)
milkPrice: org.terra.standard.market.Price[org.terra.standard.space.Volume] = 4 USD/1.0 gal

scala> val costForABunch = threeForADollar * Dozen(10)
costForABunch: org.terra.standard.market.Money = 4E+1 USD

scala> val energyCost = energyPrice * MegawattHours(4)
energyCost: org.terra.standard.market.Money = 408.8 USD

scala> val milkQuota = USD(20) / milkPrice
milkQuota: org.terra.standard.space.Volume = 5.0 gal
```

Conversions to Strings
```scala
scala> val money = USD(123.456)
money: org.terra.standard.market.Money = 123.456 USD

scala> val s = money.toString  // returns full precision amount with currency code
s: String = 123.456 USD

scala> val s = money.toFormattedString // returns currency symbol and amount rounded based on currency rules
s: String = $123.46
```

### FX Support
Currency Exchange Rates are used to define the conversion factors between currencies

```scala
scala> import market.{CurrencyExchangeRate, JPY, Money, USD}
import org.terra.standard.market.{CurrencyExchangeRate, JPY, Money, USD}

scala> // create an exchange rate
     | val rate1 = CurrencyExchangeRate(USD(1), JPY(100))
rate1: org.terra.standard.market.CurrencyExchangeRate = USD/JPY 100.0

scala> // OR
     | val rate2 = USD / JPY(100)
rate2: org.terra.standard.market.CurrencyExchangeRate = USD/JPY 100.0

scala> // OR
     | val rate3 = JPY(100) -> USD(1)
rate3: org.terra.standard.market.CurrencyExchangeRate = USD/JPY 100.0

scala> // OR
     | val rate4 = JPY(100) toThe USD(1)
rate4: org.terra.standard.market.CurrencyExchangeRate = USD/JPY 100.0

scala> val someYen: Money = JPY(350)
someYen: org.terra.standard.market.Money = 3.5E+2 JPY

scala> val someBucks: Money = USD(23.50)
someBucks: org.terra.standard.market.Money = 23.5 USD
```

Use the `convert` method which automatically converts the money to the 'other' currency:

```scala
scala> val dollarAmount: Money = rate1.convert(someYen)
dollarAmount: org.terra.standard.market.Money = 3.5 USD

scala> val yenAmount: Money = rate1.convert(someBucks)
yenAmount: org.terra.standard.market.Money = 2.35E+3 JPY
```

Or just use the `*` operator in either direction (money * rate, or rate * money):
```scala
scala> val dollarAmount2: Money = rate1 * someYen
dollarAmount2: org.terra.standard.market.Money = 3.5 USD

scala> val yenAmount2: Money = someBucks * rate1
yenAmount2: org.terra.standard.market.Money = 2.35E+3 JPY
```

### Money Context
A MoneyContext can be implicitly declared to define default settings and applicable exchange rates within its scope.
This allows your application to work with a default currency based on an application configuration or other dynamic source.
It also provides support for updating exchange rates and using those rates for automatic conversions between currencies.
The technique and frequency chosen for exchange rate updates is completely in control of the application.

Assuming these imports:
```scala
import org.terra.standard._
import energy.MegawattHours
import market.{CAD, JPY, MXN, USD}
import market.defaultMoneyContext
```

You can compute:
```scala
scala> val exchangeRates = List(USD / CAD(1.05), USD / MXN(12.50), USD / JPY(100))
exchangeRates: List[org.terra.standard.market.CurrencyExchangeRate] = List(USD/CAD 1.05, USD/MXN 12.5, USD/JPY 100.0)

scala> implicit val moneyContext = defaultMoneyContext withExchangeRates exchangeRates
moneyContext: org.terra.standard.market.MoneyContext = MoneyContext(DefaultCurrency(USD),Currencies(ARS,AUD,BRL,BTC,CAD,CHF,CLP,CNY,CZK,DKK,ETH,EUR,GBP,HKD,INR,JPY,KRW,LTC,MXN,MYR,NAD,NOK,NZD,RUB,SEK,USD,XAG,XAU,ZAR),ExchangeRates(USD/CAD 1.05,USD/JPY 100.0,USD/MXN 12.5),AllowIndirectConversions(true))

scala> val energyPrice = USD(102.20) / MegawattHours(1)
energyPrice: org.terra.standard.market.Price[org.terra.standard.energy.Energy] = 102.2 USD/1.0 MWh

scala> val someMoney = Money(350) // 350 in the default Cur
someMoney: org.terra.standard.market.Money = 3.5E+2 USD

scala> val usdMoney: Money = someMoney in USD
usdMoney: org.terra.standard.market.Money = 3.5E+2 USD

scala> val usdBigDecimal: BigDecimal = someMoney to USD
usdBigDecimal: BigDecimal = 350.0

scala> val yenCost: Money = (energyPrice * MegawattHours(5)) in JPY
yenCost: org.terra.standard.market.Money = 5.11E+4 JPY

scala> val northAmericanSales: Money = (CAD(275) + USD(350) + MXN(290)) in USD
northAmericanSales: org.terra.standard.market.Money = 635.1047619047619047619047619047619 USD
```


## How Terra works

Terra is a rewrite of the Squants library so that you can redefine the
way in which math is performed.  There are four default implementations to
give users nice default versions of the Dimensions and their types.  Terra
defines four kinds of types that are used by the library:
* T -- usually a Double, the main type used by the library
* TL -- usually a Long, the type used by whole number dimensions like 
Information
* TC -- usually a BigDecimal, the currency type
* TT -- the time type

They are:
* Standard which uses Doubles, Long, BigDecimal and Double for its four types
* Common which uses Doubles, Long, BigDecimal and Long for its four types
* Classic which uses Doubles, Double, BigDecimal and Double for its four types
* Double which uses all Doubles

To use the standard version of the library, use these imports:
```scala
import org.terra.standard._
import time.Seconds
import space.Meters
// etc...
```


To use the common version of the library, use these imports:
```scala
import org.terra.common._
import time.Seconds
import space.Meters
// etc...
```


To use the classic version of the library, use these imports:
```scala
import org.terra.classic._
import time.Seconds
import space.Meters
// etc...
```


To use the double version of the library, use these imports:
```scala
import org.terra.double._
import time.Seconds
import space.Meters
// etc...
```

## How to use Terra interactively
To use Terra interactively, run mvn scala:console in the terra directory:

```script 
git clone https://github.com/hunterpayne/terra.git terra/
cd terra
mvn scala:console
```

scala> import org.terra.standard._

## How to roll your own terra

To make your own version of Terra with your own special set of types, you
need to implement a class that extends TypeContext like StandardTuple does.
In this class you define four type aliases: T, TL, TC and TT which are the
types used for floating point, integer, currency and time types.  Then you
need to define a package object which extends TypeScope[C] where C is the 
class containing the type aliases.  Inside the TypeScope class, you define
the following:
* an implicit object which extends TerraOps[C] where C is the TypeContext
* 3 implicit vals
  * ops: TerraOps[C]
  * tag: ops.getClassTagT
  * dtag: tag.asInstanceOf[PseudoClassTag[C#T]]
* objects representing each package
* implicit classes which convert Double, Long, Int and BigDecimals to support
lifting to quantity types

Look at [StandardTerraOps.scala](https://github.com/hunterpayne/terra/tree/master/src/main/scala/org/terra/StandardTerraOps.scala), [ClassicTerraOps.scala](https://github.com/hunterpayne/terra/tree/master/src/main/scala/org/terra/ClassicTerraOps.scala), [CommonTerraOps.scala](https://github.com/hunterpayne/terra/tree/master/src/main/scala/org/terra/CommonTerraOps.scala), or [DoubleTerraOps.scala](https://github.com/hunterpayne/terra/tree/master/src/main/scala/org/terra/DoubleTerraOps.scala) for examples.

