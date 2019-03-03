
package org.terra
package market

import scala.util.Try

import org.terra.time.{ TimeLike, TimeIntegral }

/**
  * A class that represents an amount of labor performed by a person, often
  * for a Wage (which is a PricePer[Labor]).
  * @author Hunter Payne
  */
final class LaborLike[C <: TypeContext](
  val value: C#T, val unit: LaborUnit[C])(implicit ops: TerraOps[C])
    extends Quantity[LaborLike[C], C#T, C]
    with TimeIntegral[EmployeeLike[C], C#T, C#TL, C] {

  type Money = MoneyLike[C]

  import ops.laborOps._
  import ops.employeeOps.People
  import ops.timeOps.Hours

  override implicit val tttdConverter: HasConverter[C#TT, C#TL] =
    ops.converters.tttlConverter
  override implicit val ttdConverter: HasConverter[C#T, C#TL] =
    ops.converters.ttlConverter
  override def ensureTD: HasEnsureType[C#TL] = terraOps.converters.ensureTL

  def dimension: Dimension[LaborLike[C], C#T, C] = Labor
  def *(that: Price[LaborLike[C], C#T, C]): Money = that * this

  override implicit val num: Numeric[C#T] = ops.num

  protected def timeDerived = People(toPersonHours)
  protected[terra] def time = Hours(1)

  def toPersonHours = to(PersonHours)
  def toPersonDays = to(PersonDays)
  def toPersonWeeks = to(PersonWeeks)
  //def toPersonMonths = to(PersonHours)
  def toPersonYears = to(PersonYears)
}

trait LaborUnit[C <: TypeContext] extends UnitOfMeasure[LaborLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) = new LaborLike[C](t, this)
}

trait LaborOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]

  import ops.employeeOps.People

  trait LaborUnitT extends LaborUnit[C]

  object Labor extends Dimension[LaborLike[C], C#T, C] {
    private[market] def apply[A](a: A, unit: LaborUnit[C])(
      implicit num: Numeric[A]): LaborLike[C] =
      new LaborLike[C](ops.convDouble(num.toDouble(a)), unit)
    def apply(i: EmployeeLike[C], t: TimeLike[C]) = {
      implicit val tag: PseudoClassTag[C#T] = ops.getClassTagT
      PersonHours(ops.num.times(ops.rconv(i.toPeople), ops.rconvT(t.toHours)))
    }
    def apply(value: Any)(implicit ops: TerraOps[C]): Try[LaborLike[C]] =
      parse(value)
    def name = "Labor"
    def primaryUnit = PersonHours
    def siUnit = PersonHours
    def units = Set(
      PersonHours, PersonDays, PersonWeeks, /*PersonMonths,*/ PersonYears)
  }

  object PersonHours extends LaborUnitT with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = "PHr"
  }

  object PersonDays extends LaborUnitT {
    val symbol = "PDay"
    val conversionFactor = 8d
  }

  object PersonWeeks extends LaborUnitT {
    val symbol = "PWk"
    val conversionFactor = 40d
  }

  // months don't have a regular amount of hours, we could use 2000/12 which is
  // 166 2/3 but that's a non repeating decimal and the accuracy issues
  // seem to make this unit not worth supporting
  //object PersonMonths extends LaborUnitT {
    //val symbol = "PMo"
    //val conversionFactor = 160d
  //}

  object PersonYears extends LaborUnitT {
    val symbol = "PYr"
    val conversionFactor = 2000d
  }

  object LaborConversions {
    lazy val workHour = PersonHours(1)
    lazy val workDay = PersonDays(1)
    lazy val workWeek = PersonWeeks(1)
    //lazy val workMonth = PersonMonths(1)
    lazy val workYear = PersonYears(1)

    implicit class LaborConversions[A](a: A)(implicit num: Numeric[A]) {
      def personHours = PersonHours(a)
      def manHours = PersonHours(a)
      def womanHours = PersonHours(a)
      def workHours = PersonHours(a)
      def personDays = PersonDays(a)
      def manDays = PersonDays(a)
      def womanDays = PersonDays(a)
      def workDays = PersonDays(a)
      def personWeeks = PersonWeeks(a)
      def manWeeks = PersonWeeks(a)
      def womanWeeks = PersonWeeks(a)
      def workWeeks = PersonWeeks(a)
      //def personMonths = PersonMonths(a)
      def personYears = PersonYears(a)
      def manYears = PersonYears(a)
      def womanYears = PersonYears(a)
      def workYears = PersonYears(a)
    }
  }
}
