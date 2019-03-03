
package org.terra

object ClassTagType extends Enumeration {
  type ClassTagType = Value
  val TE, TLE, TCE, TTE = Value
}

import ClassTagType.ClassTagType

trait PseudoClassTag[T] {
  type U = T
  val typ: ClassTagType
}
