import scala.collection.mutable.ArrayBuffer

/**
 * Created by dimitar on 1/23/15.
 */
object FenwickTree {

  def build(arr: Array[Int]): ArrayBuffer[Int] = {
    val fenwick = new ArrayBuffer[Int](arr.length+1)

    (0 to arr.length).foreach{r=> fenwick.append(0)}
    for (i<-0 until arr.length) {
      update(fenwick, i, arr(i))
    }
    fenwick
  }

  def update(fanwick: ArrayBuffer[Int], index: Int, num: Int): Unit = {
    var idx = index + 1
    while (idx <= fanwick.length) {
      fanwick(idx) += num
      idx += idx & (-idx)
    }
  }

  def getSum(fanwick: ArrayBuffer[Int], start: Int, end: Int): Int = {
    var leftSum = 0
    var rightSum = 0

    //find left
    var left = start+1
    while (left > 0) {
      leftSum += fanwick(left)
      left -= left & (-left)
    }

    //find right
    var right = end+1
    while (right > 0) {
      rightSum += fanwick(right)
      right -= right & (-right)
    }

    if (start > 0)
      rightSum - leftSum
    else
      rightSum
  }

}
