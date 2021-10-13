import scala.annotation.tailrec
import scala.util.Random


object Main extends App {

  val bits = 8 //4 or 8
  val groupSize = (1 << bits) / bits
  val elemLength = 1 << bits

  type Elem = Seq[Boolean]
  val comb: Seq[Elem] = (0 until elemLength).map(n => n.toBinaryString.reverse.padTo(bits, '0').reverse.map(_ == '1')).to(LazyList)
  implicit class RichElem(e: Elem) {
    def dist(other: Elem): Int = other.zip(e).count(t => t._1 != t._2)
    def distIsOne(other: Elem): Boolean = dist(other) == 1
    def formatted: String = e.map(e => if (e) "1" else "0").mkString
  }

  @tailrec
  def solution(remaining: Seq[Elem], pendingGroup: Seq[Elem], completedGroups: Seq[Seq[Elem]], alreadyCovered: Set[Elem]): Seq[Seq[Elem]] = {
    if (remaining.isEmpty) {
      completedGroups
    } else {
      val next = remaining.filter(e => !comb.filter(c => c.distIsOne(e)).exists(c => alreadyCovered.contains(c))).head
      if (pendingGroup.size == groupSize - 1) {
        solution(remaining.filter(e => e != next), Seq.empty, completedGroups :+ (pendingGroup :+ next), Set.empty)
      } else {
        println(pendingGroup.size)
        solution(remaining.filter(e => e != next), pendingGroup :+ next, completedGroups, alreadyCovered ++ comb.filter(c => c.distIsOne(next)))
      }
    }
  }

  println("Solution:")
  val sol = solution(comb, LazyList.empty, LazyList.empty, Set.empty)
  sol.zipWithIndex.map(v => (v._2, v._1.map(_.formatted))).map({
    case (i, value) => s"Group $i -> ${value.mkString(", ")}"
  }).foreach(println)
  println("\nLookup table (insider):")
  comb.map(e => (e.formatted, (0 until bits).map(i => (i, sol(i).find(s => s.distIsOne(e)).get)).toList)).map({
    case (table, sol) => s"[$table]:   { " + sol.map(s => s"${s._1} => [${s._2.formatted}]").mkString(", ") + " }"
  }).foreach(println)
  println("\nLookup table (outsider):")
  comb.map(e => (e, sol.zipWithIndex.find(v => v._1.contains(e)).get._2)).map({
    case (e, i) => s"[${e.formatted}] -> $i"
  }).foreach(println)
}
