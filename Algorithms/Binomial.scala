package hackercup2015.round1

/**
 * Calculates n choose k mod some_prime using Fermat's little theorem.
 */
object Binomial {

  def combinations(n: Int, k: Int, p: Long): Long = {
    var res = 1L
    for (i <- n until n - k by -1){
      res = res * i % p
    }

    for (i <- 1 to k) {
      res = res * inverse(i, p-2, p) %p
    }
    res
  }

  def inverse(a: Long, b: Long, mod: Long): Long = {
    var x = 1L
    var y = a
    var t = b

    while (t > 0) {
      if(t%2 == 1) {
        x= x*y  % mod
      }

      y =  y*y % mod
      t = t / 2
    }
    return x
  }
}


