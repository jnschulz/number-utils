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

    it("should produce general number streams by index (e.g. evens) correctly") {
      val evens = NumberStreams.getNumberStream(n => 2*n)
      evens.take(5).toList should equal (List(2, 4, 6, 8, 10))
    }

    it("should produce general number streams by index (e.g. squares) correctly") {
      val squares = NumberStreams.getNumberStream(n => n*n)
      squares.take(5).toList should equal (List(1, 4, 9, 16, 25))
    }

    it("should produce general number streams by iteration over one value (e.g. evens) correctly") {
      val evens = NumberStreams.getNumberStream(2L, (n: Long) => n + 2L)
      evens.take(5).toList should equal (List(2, 4, 6, 8, 10))
    }

    it("should produce general number streams by iteration over two value (e.g. fibs) correctly") {
      val fibs = NumberStreams.getNumberStream(0L, 1L, (n1: Long, n2: Long) => n1 + n2)
      fibs.take(7).toList should equal (List(0, 1, 1, 2, 3, 5, 8))
    }
  }
}
