package com.jnschulz.numberutils

object Factors {
  def divisorsOf(num: Long): Set[Long] = {
    (1L to Math.sqrt(num).toLong).filter(num % _ == 0).map {
      x => x :: num/x :: Nil
    }.flatten.toSet
  }

  def distinctPrimeFactorsOf(num: Long): Set[Long] = {
    divisorsOf(num).filter(Primes.isPrime)
  }
}
