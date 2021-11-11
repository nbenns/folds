object Step5 extends App {
  def foldList[ELEM, COMB](process: (List[ELEM] => COMB) => List[ELEM] => COMB): List[ELEM] => COMB = { list =>
    process(foldList(process))(list)
  }

  def unfoldList[ELEM, COMB](process: (COMB => List[ELEM]) => COMB => List[ELEM]): COMB => List[ELEM] = { seed =>
    process(unfoldList(process))(seed)
  }

  def sum(next: List[Int] => Int): List[Int] => Int = {
    case Nil    => 0
    case h :: t => h + next(t)
  }

  def upto10(next: Int => List[Int]): Int => List[Int] = { seed =>
    if (seed < 10) seed :: next(seed + 1)
    else Nil
  }

  val sumRes = sum(_ => 100)(List(1, 2, 3))
  val sum2Res = sum(sum(_ => 100))(List(1, 2, 3))
  val sum3Res = sum(sum(sum(_ => 100)))(List(1, 2, 3))
  val sum4Res = sum(sum(sum(sum(_ => 100))))(List(1, 2, 3))

  println(sumRes)
  println(sum2Res)
  println(sum3Res)
  println(sum4Res)

  println()

  val foldRes = foldList(sum)(List(1, 2, 3))
  val unfoldRes = unfoldList(upto10)(0)

  println(foldRes)
  println(unfoldRes)
}
