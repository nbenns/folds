import scala.annotation.tailrec
import scala.collection.immutable.TreeMap

object Step1 extends App {
  def foldLeft[ELEM, COMB](nil: COMB, cons: (COMB, ELEM) => COMB): List[ELEM] => COMB = {
    case Nil          => nil
    case head :: tail => foldLeft(cons(nil, head), cons)(tail)
  }

  def foldRight[ELEM, COMB](nil: COMB, cons: (ELEM, COMB) => COMB): List[ELEM] => COMB = {
    case Nil           => nil
    case elem :: listr => cons(elem, foldRight(nil, cons)(listr))
  }

  val foldRes = foldLeft[Int, Int](0, _ + _)(List(1, 2, 3))
  println(foldRes)
}
