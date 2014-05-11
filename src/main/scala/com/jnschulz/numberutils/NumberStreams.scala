package com.jnschulz.numberutils

object NumberStreams {
  def naturalNumbers: Stream[Long] = getNumberStream(x => x)

  def getNumberStream(f:(Long) => Long): Stream[Long] = {
    def loop(n: Long): Stream[Long] = f(n) #:: loop(n + 1L)
    loop(1L)
  }

  def getNumberStream[T](firstValue: T, f:(T) => T): Stream[T] = {
    def loop(nMinus1: T): Stream[T] = {
      val n = f(nMinus1)
      n #:: loop(n)
    }
    firstValue #:: loop(firstValue)
  }

  def getNumberStream[T](firstValue: T, secondValue: T, f:(T, T) => T): Stream[T] = {
    def loop(nMinus1: T, nMinus2: T): Stream[T] = {
      val n = f(nMinus1, nMinus2)
      n #:: loop(n, nMinus1)
    }
    firstValue #:: secondValue #:: loop(secondValue, firstValue)
  }

  def fibs: Stream[BigInt] = NumberStreams.getNumberStream(BigInt(0), BigInt(1), (n1: BigInt, n2: BigInt) => n1 + n2)

}
