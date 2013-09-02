package com.jnschulz.numberutils

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NumberStreamsTest extends FunSpec with ShouldMatchers {
  describe("NumberStreams") {
    it("should produce natural numbers correctly") {
      NumberStreams.naturalNumbers.take(100).toList should equal ((1L to 100L).toList)
    }

    it("should produce fibonacci numbers correctly") {
      NumberStreams.fibs.take(10).toList should equal (List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34).map(BigInt(_)))
    }

    it("should produce triangular numbers correctly") {
      NumberStreams.triangularNumbers.take(5).toList should equal (List(1, 3, 6, 10, 15))
    }

    it("should produce pentagonal numbers correctly") {
      NumberStreams.pentagonalNumbers.take(5).toList should equal (List(1, 5, 12, 22, 35))
    }

    it("should produce hexagonal numbers correctly") {
      NumberStreams.hexagonalNumbers.take(5).toList should equal (List(1, 6, 15, 28, 45))
    }
  }
}
