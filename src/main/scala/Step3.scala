object Step3 extends App {
  def foldList[ELEM, COMB](out: Option[(ELEM, COMB)] => COMB): List[ELEM] => COMB = {
    case Nil          => out(None)
    case head :: tail => out(Some((head, foldList(out)(tail))))
  }

  def unfoldList[ELEM, COMB](into: COMB => Option[(ELEM, COMB)]): COMB => List[ELEM] = { seed =>
    into(seed) match {
      case None               => Nil
      case Some((head, next)) => head :: unfoldList(into)(next)
    }
  }


  val sum: Option[(Int, Int)] => Int = {
    case Some(a, b) => a + b
    case None => 0
  }

  val upto10: Int => Option[(Int, Int)] = { seed =>
    if (seed < 10) Some((seed, seed + 1))
    else None
  }

  val foldRes: Int = foldList(sum)(List(1, 2, 3))
  val unfoldRes: List[Int] = unfoldList(upto10)(0)

  println(foldRes)
  println(unfoldRes)

  def foldOption[ELEM, COMB](out: Option[ELEM] => COMB): Option[ELEM] => COMB = {
    case None       => out(None)
    case Some(elem) => out(Some(elem))
  }

  def unfoldOption[ELEM, COMB](into: COMB => Option[ELEM]): COMB => Option[ELEM] = { seed =>
    into(seed)
  }

  def foldEither[ELEML, ELEMR, COMB](out: Option[ELEML | ELEMR] => COMB): Either[ELEML, ELEMR] => COMB = {
    case Left(eleml)  => out(Some(eleml))
    case Right(elemr) => out(Some(elemr))
  }

  def unfoldEither[ELEML, ELEMR, COMB](into: COMB => Option[ELEML | ELEMR]): COMB => Either[ELEML, ELEMR] = { seed =>
    into(seed) match {
      case None => ???
      case l: Some[ELEML] => Left(l.value)
      case r: Some[ELEMR] => Right(r.value)
    }
  }
}
