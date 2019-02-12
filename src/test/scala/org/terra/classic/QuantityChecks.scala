
package org.terra.classic

import org.scalacheck.Gen

import time.Seconds

trait QuantityChecks {

  type TestData = Int
  val posNum: Gen[TestData] = Gen.posNum[TestData]
  val tol = 1e-13
  implicit val tolTime = Seconds(tol)
}
