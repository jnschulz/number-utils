package com.jnschulz.numberutils

import collection.immutable.SortedMap

object Primes {
 def isPrime(num: Long): Boolean = {
    if (num <= 1) {
      false
    } else if (num == 2) {
      true
    } else {
      (2L to Math.sqrt(num).toLong).forall( num % _ != 0 )
    }
  }

  def primeNumbers: Stream[Long] = {
    def addIncrementer(incrementerMap: SortedMap[Long, List[Long]], seed: Long, currentIncrement: Long) = {
      val seeds = seed :: incrementerMap.getOrElse(currentIncrement, List())
      incrementerMap.updated(currentIncrement, seeds)
    }

    def loop(v: Long, incrementerMap: SortedMap[Long, List[Long]]): Stream[Long] = {
      val lowestKnownComposite = incrementerMap.head._1
      if (lowestKnownComposite > v) {
        //Add to the map and recurse
        v #:: loop(v + 1L, addIncrementer(incrementerMap, v, v*v))
      } else {
        assert(v == lowestKnownComposite)

        //Increment the map and recurse
        val seeds = incrementerMap.get(v).get
        val newIncrementerMap = seeds.foldLeft(incrementerMap - v){
          (map, seed) => addIncrementer(map, seed, v + seed)
        }

        loop(v + 1L, newIncrementerMap)
      }
    }

    2L #:: loop(3L, SortedMap(4L -> List(2L)))
  }

  /** Not an FP method, but pretty fast... */
  def primeNumbersTo(max: Long): List[Long] = {
    assert(max <= Int.MaxValue)
    var candidates = (0L to max).toVector.map( _ >= 2L )
    var primes = List[Long]()
    (0L to max).foreach {
      index =>
      //If it's a prime...
        if (candidates(index.toInt)) {
          primes ::= index
          (index*index to max by index).foreach{ i:Long =>
            if (i >= 0 && i <= max)
              candidates = candidates.updated(i.toInt, false)
          }
        }
    }

    primes.reverse
  }
}
