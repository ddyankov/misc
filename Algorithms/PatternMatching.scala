/**
 * Created by dimitar on 11/30/14.
 *
 * "Given a pattern and a string input - find if the string follows the same pattern and return 0 or 1.
    Examples:
1) Pattern : "abab", input: "redblueredblue" should return 1.
2) Pattern: "aaaa", input: "asdasdasdasd" should return 1.
3) Pattern: "aabb", input: "xyzabcxzyabc" should return 0.
 */

object PatternMatching {

  def main(args: Array[String]) {

    val ans = scala.collection.mutable.Set.empty[Int]
    doSplit("redblueredblue", "abab", "", 0, ans)

    if (ans.isEmpty)
      println("0")
    else
      println("1")
  }

  def doSplit(str: String, pattern: String, res: String, split: Int, ans: scala.collection.mutable.Set[Int]): Unit = {
    if (split == pattern.length - 1) {
      val clean = (res+" "+str).trim.replaceAll(" ", ",")
      if (check(clean, pattern)) {
        ans.add(1)
        return
      }
    } else {
      for(i <- 0 until str.length) {
        doSplit(str.drop(i+1), pattern, res+" "+str.take(i+1), split+1, ans)
      }
    }
  }

  def check(str: String, pattern: String): Boolean = {
    val in = str.split(",").filter(c => !c.trim.isEmpty)
    if (in.length != pattern.length) {
      return false
    }
    var res = true
    var keep = true
    val map = scala.collection.mutable.Map.empty[String, String]

    for(i <- 0 until pattern.length if keep) {
      val cmp = in(i).trim
      val key = pattern.charAt(i).toString
      if (!map.contains(key) && !map.contains(cmp)) {
        map(key.toString) = cmp
        map(cmp) = key.toString
      } else if(!map.getOrElse(key,"").equals(cmp) && !map.getOrElse(cmp, "").equals(key.toString)) {
          res = false
          keep = false
      }
    }
    res
  }
}
