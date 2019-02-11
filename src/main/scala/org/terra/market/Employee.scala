
package org.terra
package market

import scala.reflect.ClassTag

import org.terra.time.{ TimeLike, TimeDerivative }

/**
  * An employee class representing a person providing labor for some effort.
  * This class uses the whole number type (like Information) which means for
  * some uses, accuracy can be an issue.
  * @author Hunter Payne
  */
case class EmployeeLike[C <: TypeContext](
  val value: C#TL, val unit: EmployeeUnit[C])(implicit ops: TerraOps[C])
    extends Quantity[EmployeeLike[C], C#TL, C] 
    with TimeDerivative[LaborLike[C], C#TL, C#T, C] {

  override implicit val tttiConverter: HasConverter[C#TT, C#T] =
    terraOps.converters.tttConverter2

  override def makeEnsureType(implicit ops: TerraOps[C]): HasEnsureType[C#TL] =
    ops.converters.ensureTL
  override def makeFromCurrencyConverter(
    implicit ops: TerraOps[C]): HasConverter[C#TC, C#TL] =
    ops.converters.tctlConverter
  override def makeToCurrencyConverter(
    implicit ops: TerraOps[C]): HasConverter[C#TL, C#TC] =
    ops.converters.tltcConverter

  override def getNumeric: Numeric[C#TL] = ops.numL
  override def getTag: ClassTag[C#TL] = ops.getClassTagTL

  import ops.employeeOps._
  import ops.laborOps.PersonHours
  import ops.timeOps.Hours

  def dimension: Dimension[EmployeeLike[C], C#TL, C] = Employee
  def convLong(l: Long)(implicit ops: TerraOps[C]): C#TL = l.asInstanceOf[C#TL]

  protected[terra] def timeIntegrated = PersonHours(toPeople)
  protected[terra] def time = Hours(1)

  def toPeople = to(People)
}

trait EmployeeUnit[C <: TypeContext] 
    extends UnitOfMeasure[EmployeeLike[C], C#TL, C] with UnitConverter[C#TL, C] {
  def apply(t: C#TL)(
    implicit tag: ClassTag[C#TL], ops: TerraOps[C]): EmployeeLike[C] =
    new EmployeeLike[C](t, this)
  override def apply[A](a: A)(
    implicit num: Numeric[A], ops: TerraOps[C]): EmployeeLike[C] = {
    implicit val tag = getTag
    new EmployeeLike[C](ops.convLong(num.toLong(a)), this)
  }

  override def makeEnsureType(implicit ops: TerraOps[C]): HasEnsureType[C#TL] =
    ops.converters.ensureTL

  override private[terra] def makeDoubleConverter(
    implicit ops: TerraOps[C]): HasConverter[Double, C#TL] =
    ops.converters.dtlConverter.asInstanceOf[HasConverter[Double, C#TL]]
  override private[terra] def makeLongConverter(
    implicit ops: TerraOps[C]): HasConverter[Long, C#TL] = 
    ops.converters.ltlConverter.asInstanceOf[HasConverter[Long, C#TL]]
  override def getTag(implicit ops: TerraOps[C]): ClassTag[C#TL] =
    ops.getClassTagTL
}

trait EmployeeOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]

  def convLong(l: Long)(implicit ops: TerraOps[C]): C#TL
  implicit val tagTL: ClassTag[C#TL]

  trait EmployeeUnitT extends EmployeeUnit[C]

  /**
    * Factory singleton for employee
    */
  object Employee extends Dimension[EmployeeLike[C], C#TL, C] 
      with BaseDimension {

    private[market] def apply[A](a: A, unit: EmployeeUnit[C])(
      implicit n: Numeric[A]) =
      new EmployeeLike[C](convLong(n.toLong(a)), unit)
    def apply(value: Any) = parseL(value)
    def name = "Employee"
    def primaryUnit = People
    def siUnit = People
    def units = Set(People)
    def dimensionSymbol = "P"
  }

  object People extends EmployeeUnitT with PrimaryUnit[C#TL, C]
      with SiBaseUnit {
    val symbol = "P"
  }

  object EmployeeConversions {

    lazy val person = People(1)

    implicit class EmployeeConversions[A](a: A)(implicit na: Numeric[A]) {
      def people = People(a)
    }

    //implicit class EmployeeStringConversions(s: String) {
      //def toEmployees = Employee(s)
    //}
  }
}

