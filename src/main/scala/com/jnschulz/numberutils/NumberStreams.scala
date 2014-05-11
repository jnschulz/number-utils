package com.jnschulz.numberutils

object NumberStreams {
  def naturalNumbers: Stream[Long] = getNumberStream(x => x)

  def getNumberStream(f:(Long) => Long): Stream[Long] = {
    def loop(n: Long): Stream[Long] = f(n) #:: loop(n + 1L)
    loop(1L)
  }

  lazy val fibs: Stream[BigInt] = BigInt(0L) #:: BigInt(1L) #:: fibs.zip(fibs.tail).map{ case (x1, x2) => x1 + x2 }
}
