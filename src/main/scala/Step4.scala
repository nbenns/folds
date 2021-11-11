object Step4 extends App {
  def foldList[ELEM, COMB](process: List[ELEM] => COMB): List[ELEM] => COMB =
    process

  def unfoldList[ELEM, COMB](process: COMB => List[ELEM]): COMB => List[ELEM] =
    process


  val sum: List[Int] => Int = {
    case Nil    => 0
    case h :: t => h + sum(t)
  }

  val upto10: Int => List[Int] = { seed =>
    if (seed < 10) seed :: upto10(seed + 1)
    else Nil
  }

  val foldRes = foldList(sum)(List(1, 2, 3))
  val unfoldRes = unfoldList(upto10)(0)

  println(foldRes)
  println(unfoldRes)
}
