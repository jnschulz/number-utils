package com.jnschulz.numberutils

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PrimesTest extends FunSpec with ShouldMatchers {
  describe("Primes") {
    it("should produce primes correctly") {
      Primes.primeNumbers.take(10).toList should equal {
        List(2L, 3L, 5L, 7L, 11L, 13L, 17L, 19L, 23L, 29L)
      }
    }

    it("should produce detect primes correctly") {
      (1L to 30L).filter(Primes.isPrime(_)).toList should equal {
        List(2L, 3L, 5L, 7L, 11L, 13L, 17L, 19L, 23L, 29L)
      }
    }
  }
}
