/**
  * F(n) = 1*F(n-1) + 2*F(n-2) + 3*F(n-3)
  */
object MatrixExponentiation {

  def findNth(n: Long): Long = {
    if (n == 1 || n == 2 || n == 3) {
      return n
    }
    val F: Array[Array[Long]] = Array(Array(1,2,3), Array(1,0,0), Array(0,1,0))
    return power(F, n-2)
  }

  val M: Array[Array[Long]] = Array(Array(1,2,3), Array(1,0,0), Array(0,1,0))

  def multiply(a: Array[Array[Long]], b: Array[Array[Long]]) = {
    val mul = Array.ofDim[Long](3,3)

    for (i<-0 until 3) {
      for (j<-0 until 3) {
        mul(i)(j) = 0
        for (k<-0 until 3)
          mul(i)(j) += a(i)(k)*b(k)(j)
        val t = mul(i)(j)
        if ( t % 2 == 0) {
          mul(i)(j) = 2
        } else {
          mul(i)(j) = 3
        }
      }
    }

    for (i<-0 until 3)
      for (j<-0 until 3)
        a(i)(j) = mul(i)(j)
  }

  def power(F: Array[Array[Long]], n: Long): Long = {
    if (n==1) {
      return 3*F(1)(0) + 2*F(1)(1) + F(1)(2)
    }

    power(F, n/2)
    multiply(F, F)

    if (n % 2 != 0) {
      multiply(F, M)
    }
    return 3*F(1)(0) + 2*F(1)(1) + F(1)(2)
  }
  
}
