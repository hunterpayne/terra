
package test

import scalanative.native._, stdlib._

import caseapp._

case class TerraOptions(name: String)

object Main extends CaseApp[TerraOptions] {

  // needed as scala-native at the moment does not import properly the 
  // main from CaseApp
  override def main(args: Array[String]) = super.main(args)

  def run(options: TerraOptions, arg: RemainingArgs): Unit = {
    println(s"Hello, ${options.name}!")
  }
}
