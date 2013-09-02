package com.jnschulz.numberutils

object NumberStreams {
  def naturalNumbers: Stream[Long] = {
    def loop(n: Long): Stream[Long] = n #:: loop(n + 1L)
    loop(1L)
  }

  lazy val fibs: Stream[BigInt] = BigInt(0L) #:: BigInt(1L) #:: fibs.zip(fibs.tail).map{ case (x1, x2) => x1 + x2 }

  def triangularNumbers: Stream[Long] = {
    def loop(n: Long): Stream[Long] = (n*(n + 1L)/2L) #:: loop(n + 1L)
    loop(1L)
  }

  def pentagonalNumbers: Stream[Long] = {
    def loop(n: Long): Stream[Long] = (n*(3L*n - 1L)/2L) #:: loop(n + 1L)
    loop(1L)
  }

  def hexagonalNumbers: Stream[Long] = {
    def loop(n: Long): Stream[Long] = (n*(2L*n - 1L)) #:: loop(n + 1L)
    loop(1L)
  }
}
