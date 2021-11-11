object Step6 extends App {
  def fold[F[_], ELEM, COMB](process: (F[ELEM] => COMB) => F[ELEM] => COMB): F[ELEM] => COMB = { structure =>
    process(fold(process))(structure)
  }

  def unfold[F[_], ELEM, COMB](process: (COMB => F[ELEM]) => COMB => F[ELEM]): COMB => F[ELEM] = { seed =>
    process(unfold(process))(seed)
  }


  def sum(next: List[Int] => Int): List[Int] => Int = {
    case Nil    => 0
    case h :: t => h + next(t)
  }

  def upto10(next: Int => List[Int]): Int => List[Int] = { seed =>
    if (seed < 10) seed :: next(seed + 1)
    else Nil
  }

  val foldRes: Int = fold(sum)(List(1, 2, 3))
  val unfoldRes: List[Int] = unfold(upto10)(0)

  println(foldRes)
  println(unfoldRes)
}
