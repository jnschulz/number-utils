package com.jnschulz.numberutils

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FactorsTest extends FunSpec with ShouldMatchers {
  describe("Factors") {
    it("should compute divisors correctly") {
      Factors.divisorsOf(1L) should equal (Set(1L))
      Factors.divisorsOf(2L) should equal (Set(1L, 2L))
      Factors.divisorsOf(12L) should equal (Set(1L, 2L, 3L, 4L, 6L, 12L))
      Factors.divisorsOf(25L) should equal (Set(1L, 5L, 25L))
    }

    it("should compute distinct prime factors correctly") {
      Factors.distinctPrimeFactorsOf(1L) should equal (Set())
      Factors.distinctPrimeFactorsOf(2L) should equal (Set(2L))
      Factors.distinctPrimeFactorsOf(14L) should equal (Set(2L, 7L))
      Factors.distinctPrimeFactorsOf(15L) should equal (Set(3L, 5L))
      Factors.distinctPrimeFactorsOf(644L) should equal (Set(2L, 7L, 23L))
      Factors.distinctPrimeFactorsOf(645L) should equal (Set(3L, 5L, 43L))
      Factors.distinctPrimeFactorsOf(646L) should equal (Set(2L, 17L, 19L))
    }
  }
}
