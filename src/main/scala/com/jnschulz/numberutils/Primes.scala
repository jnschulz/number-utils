package com.jnschulz.numberutils

import collection.immutable.SortedMap
import gnu.trove.set.hash.TLongHashSet
import gnu.trove.set.TLongSet
import gnu.trove.procedure.TLongProcedure

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

  private[numberutils] def primeNumbersSlower: Stream[Long] = {
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
  def primeNumbers: Stream[Long] = {
    val primesEmitted: TLongSet = new TLongHashSet()

    //Logically consistent but worthless initial values that can be operated on by reinitialize()
    var intervalLength: Int = 1
    var minCandidate: Long = 1L
    var maxCandidate: Long = 1L
    var candidates: Array[Boolean] = Array(false)

    reinitialize()

    def reinitialize() {
      //Make a new array of candidates twice the size of the previous to hold the next interval of candidates
      intervalLength *= 2
      minCandidate = maxCandidate + 1
      maxCandidate = maxCandidate + intervalLength
      candidates = Array.fill[Boolean]( intervalLength )(true)

      //Run the candidates through the sieve of previously emitted primes
      primesEmitted.forEach(new TLongProcedure {
        override def execute(prime: Long): Boolean = {
          updateCandidates(prime)
          true
        }
      })
    }

    def updateCandidates(prime: Long) {
      //Start at either prime^2 or the first multiple of prime in the candidate interval
      val initialComposite = (prime*prime) max ( if (minCandidate % prime == 0) minCandidate else minCandidate + prime - (minCandidate % prime) )

      //Mark the multiples of this prime as invalid candidates
      (initialComposite to maxCandidate by prime).foreach { i: Long =>
        if (i >= 0 && i <= maxCandidate)
          candidates( (i - minCandidate).toInt) = false
      }
    }

    def loop(candidate: Long): Stream[Long] = {
      //Reinitialize if we have run out of candidates
      if (candidate > maxCandidate) {
        reinitialize()
      }

      if ( candidates( (candidate - minCandidate).toInt ) ) {
        //Emit and update the candidates before looping
        val prime = candidate
        updateCandidates(prime)
        primesEmitted.add(prime)
        prime #:: loop(candidate + 1L)
      } else {
        //Simply loop
        loop(candidate + 1L)
      }
    }

    loop(minCandidate)
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
