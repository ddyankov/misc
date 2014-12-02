/**
 * Created by dimitar on 11/26/14.
 */
object QuickSort {

  def main(args: Array[String]) {
    val list = List(10,15,1,2,6,100)
    println(quicksort(list))
  }

  def quicksort(xs: List[Int]): List[Int] = {
    if(xs.length < 1) {
      xs
    } else {
      val pivot = xs(xs.length/2)
        quicksort(xs.filter(_ < pivot)) ::: quicksort(xs.filter(_ > pivot))
    }
  }
}
