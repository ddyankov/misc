/**
 * Created by dimitar on 11/26/14.
 */
object MergeSort {

  def main(args: Array[String]) {
    val list = List(10,15,1,2,6,100)
    println(sort(list))
  }


  def sort(xs: List[Int]): List[Int] = {
    val h = xs.length/2

    if (h==0)
      xs
    else {
      val (h1,h2) = xs splitAt h
      merge(sort(h1), sort(h2))
    }
  }

  def merge(xs: List[Int], ys: List[Int]): List[Int] = {
    (xs, ys) match {
      case (Nil, _) => ys
      case (_, Nil) => xs
      case (x::xs1, y::ys1) => if (x < y) x :: merge(xs1, ys) else y :: merge(xs, ys1)
    }
  }
}
