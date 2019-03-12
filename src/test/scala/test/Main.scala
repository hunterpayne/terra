
package test

import scala.concurrent.duration.{ DAYS, Duration, HOURS, MICROSECONDS, MILLISECONDS, MINUTES, NANOSECONDS, SECONDS }

import caseapp._
import org.scalatest._
import org.scalatest.events._

import org.terra.{ QuantityParseException, MetricSystem }
import org.terra.standard._
import org.terra.standard.time._
import org.terra.standard.space._
import org.terra.standard.motion._
import org.terra.standard.information._
import org.terra.standard.radio.SquareMeterSeconds
import org.terra.standard.mass.Kilograms
import org.terra.standard.photo.{ LumenSeconds, Lumens, Lux, LuxSeconds }
import org.terra.standard.electro.{ Amperes, Coulombs, Volts, Webers }
import org.terra.standard.energy.{ WattHours, Watts, WattsPerHour }

case class TerraOptions(name: Option[String])

object Main extends CaseApp[TerraOptions] {

  // needed as scala-native at the moment does not import properly the 
  // main from CaseApp
  override def main(args: Array[String]) = super.main(args)

  def run(options: TerraOptions, arg: RemainingArgs): Unit = {
    dimensionlessSpec
    timeSpec
    frequencySpec
  }

  def dimensionlessSpec: Unit = {

    assert(Percent(1).toPercent == 1)
    assert(Each(1).toEach == 1)
    assert(Dozen(1).toDozen == 1)
    assert(Score(1).toScore == 1)
    assert(Gross(1).toGross == 1)

    assert(Dimensionless("10.22 %").get == Percent(10.22))
    assert(Dimensionless("10.22 ea").get == Each(10.22))
    assert(Dimensionless("10.22 dz").get == Dozen(10.22))
    assert(Dimensionless("10.22 score").get == Score(10.22))
    assert(Dimensionless("10.22 gr").get == Gross(10.22))
    assert(Dimensionless("10.45 zz").failed.get == QuantityParseException("Unable to parse Dimensionless", "10.45 zz"))
    assert(Dimensionless("zz ea").failed.get == QuantityParseException("Unable to parse Dimensionless", "zz ea"))

    val x = Gross(1)
    assert(x.toPercent == 14400)
    assert(x.toEach == 144)
    assert(x.toDozen == 12)
    assert(x.toScore == (144d / 20))
    assert(x.toGross == 1)

    assert(Percent(1).toString(Percent) == "1.0 %")
    assert(Each(1).toString(Each) == "1.0 ea")
    assert(Dozen(1).toString(Dozen) == "1.0 dz")
    assert(Score(1).toString(Score) == "1.0 score")
    assert(Gross(1).toString(Gross) == "1.0 gr")

    assert((Each(2) * Dozen(1)) == Dozen(2))
    assert((Dozen(5) * Percent(10)) == Each(6))
    assert((Each(10) + 10.22) == Each(20.22))
    assert((Each(60) / Seconds(1)) == Hertz(60))
    assert((Each(60) / Hertz(60)) == Seconds(1))

    println("DimensionlessSpec worked")    
  }

  def timeSpec: Unit = {

    assert(Nanoseconds(1).toNanoseconds == 1)
    assert(Microseconds(1).toMicroseconds == 1)
    assert(Milliseconds(1).toMilliseconds == 1)
    assert(Seconds(1).toSeconds == 1)
    assert(Minutes(1).toMinutes == 1)
    assert(Hours(1).toHours == 1)
    assert(Days(1).toDays == 1)

    assert(Time("10.22 ns").get == Nanoseconds(10.22))
    assert(Time("10.22 µs").get == Microseconds(10.22))
    assert(Time("10.22 ms").get == Milliseconds(10.22))
    assert(Time("10.22 s").get == Seconds(10.22))
    assert(Time("10.22 m").get == Minutes(10.22))
    assert(Time("10.22 h").get == Hours(10.22))
    assert(Time("10.22 d").get == Days(10.22))
    assert(Time("10.22 z").failed.get == QuantityParseException("Unable to parse Time", "10.22 z"))
    assert(Time("ZZ ms").failed.get == QuantityParseException("Unable to parse Time", "ZZ ms"))

    assert(Time(Duration(10, NANOSECONDS)) == Nanoseconds(10))
    assert(Time(Duration(10, MICROSECONDS)) == Microseconds(10))
    assert(Time(Duration(10, MILLISECONDS)) == Milliseconds(10))
    assert(Time(Duration(10, SECONDS)) == Seconds(10))
    assert(Time(Duration(10, MINUTES)) == Minutes(10))
    assert(Time(Duration(10, HOURS)) == Hours(10))
    assert(Time(Duration(10, DAYS)) == Days(10))

    assert(Nanoseconds(3000) == Microseconds(3))
    assert(Microseconds(1000) == Milliseconds(1))
    assert(Milliseconds(1000) == Seconds(1))
    assert(Seconds(60) == Minutes(1))
    assert(Minutes(60) == Hours(1))
    assert(Hours(24) == Days(1))

    assert(Seconds(60) + Minutes(1) == Minutes(2))
    assert(Minutes(15) + Hours(1.75) == Hours(2))

    assert(Minutes(5) - Seconds(30) == Minutes(4.5))
    assert(Hours(5) - Minutes(30) == Hours(4.5))
    assert(Days(5) - Hours(12) == Days(4.5))

    val x = Seconds(1)
    assert(x.toNanoseconds == 1e9)
    assert(x.toMicroseconds == 1e6)
    assert(x.toMilliseconds == 1000)
    assert(x.toSeconds == 1)
    assert(x.toMinutes == 1d / 60)
    assert(x.toHours == 1d / 3600)
    assert(x.toDays == 1d / (3600 * 24))

    assert(Nanoseconds(1).toString(Nanoseconds) == "1.0 ns")
    assert(Microseconds(1).toString(Microseconds) == "1.0 µs")
    assert(Milliseconds(1).toString(Milliseconds) == "1.0 ms")
    assert(Seconds(1).toString(Seconds) == "1.0 s")
    assert(Minutes(1).toString(Minutes) == "1.0 m")
    assert(Hours(1).toString(Hours) == "1.0 h")
    assert(Days(1).toString(Days) == "1.0 d")

    assert(Seconds(1) * MetersPerSecond(1) == Meters(1))
    assert(Seconds(1) * MetersPerSecondSquared(1) == MetersPerSecond(1))
    assert(Seconds(1) * MetersPerSecondCubed(1) == MetersPerSecondSquared(1))

    assert(Seconds(1) * SquareMeters(1) == SquareMeterSeconds(1))
    println("TimeSpec worked")    
  }

  def frequencySpec: Unit = {

    assert(Hertz(1).toHertz == 1)
    assert(Kilohertz(1).toKilohertz == 1)
    assert(Megahertz(1).toMegahertz == 1)
    assert(Gigahertz(1).toGigahertz == 1)
    assert(Terahertz(1).toTerahertz == 1)
    assert(RevolutionsPerMinute(1).toRevolutionsPerMinute == 1)

    assert(Frequency("10.22 Hz").get == Hertz(10.22))
    assert(Frequency("10.22 kHz").get == Kilohertz(10.22))
    assert(Frequency("10.22 MHz").get == Megahertz(10.22))
    assert(Frequency("10.22 GHz").get == Gigahertz(10.22))
    assert(Frequency("10.22 THz").get == Terahertz(10.22))
    assert(Frequency("10.22 rpm").get == RevolutionsPerMinute(10.22))
    assert(Frequency("10.45 zz").failed.get == QuantityParseException("Unable to parse Frequency", "10.45 zz"))
    assert(Frequency("zz Hz").failed.get == QuantityParseException("Unable to parse Frequency", "zz Hz"))

    val x = Hertz(1)
    assert(x.toHertz == 1)
    assert(x.toKilohertz == 1 / MetricSystem.Kilo)
    assert(x.toMegahertz == 1 / MetricSystem.Mega)
    assert(x.toTerahertz == 1 / MetricSystem.Tera)
    assert(x.toRevolutionsPerMinute == 60)

    assert(Hertz(1).toString(Hertz) == "1.0 Hz")
    assert(Kilohertz(1).toString(Kilohertz) == "1.0 kHz")
    assert(Megahertz(1).toString(Megahertz) == "1.0 MHz")
    assert(Gigahertz(1).toString(Gigahertz) == "1.0 GHz")
    assert(Terahertz(1).toString(Terahertz) == "1.0 THz")
    assert(RevolutionsPerMinute(1).toString(RevolutionsPerMinute) == "1.0 rpm")

    assert(Hertz(1) * Seconds(1) == Each(1))

    assert(Hertz(100) * FeetPerSecondSquared(100) == FeetPerSecondCubed(10000))
    assert(Hertz(100) * Radians(100) == RadiansPerSecond(10000))
    assert(Hertz(100) * Each(100) == Hertz(10000))
    assert(Hertz(100) * Coulombs(100) == Amperes(10000))
    assert(Hertz(100) * WattHours(100) == Watts(36000000))
    assert(Hertz(100) * Newtons(100) == NewtonsPerSecond(10000))
    assert(Hertz(100) * Bytes(100) == BytesPerSecond(10000))
    assert(Hertz(100) * Meters(100) == MetersPerSecond(10000))
    assert(Hertz(100) * LumenSeconds(100) == Lumens(10000))
    assert(Hertz(100) * LuxSeconds(100) == Lux(10000))
    assert(Hertz(100) * Webers(100) == Volts(10000))
    assert(Hertz(100) * Kilograms(100) == KilogramsPerSecond(10000))
    assert(Hertz(100) * NewtonSeconds(100) == Newtons(10000))
    assert(Hertz(100) * Watts(100) == WattsPerHour(36000000))
    assert(Hertz(100) * Pascals(100) == PascalsPerSecond(10000))
    assert(Hertz(100) * MetersPerSecond(100) == MetersPerSecondSquared(10000))
    assert(Hertz(100) * CubicMeters(100) == CubicMetersPerSecond(10000))    
    println("FrequencySpec worked")    
  }
}
