package com.jnschulz.numberutils

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FactorsTest extends FunSpec with ShouldMatchers {
  describe("Factors") {
    it("should computer divisors correctly") {
      Factors.divisorsOf(1L) should equal (Set(1L))
      Factors.divisorsOf(2L) should equal (Set(1L, 2L))
      Factors.divisorsOf(12L) should equal (Set(1L, 2L, 3L, 4L, 6L, 12L))
      Factors.divisorsOf(25L) should equal (Set(1L, 5L, 25L))
    }
  }
}
