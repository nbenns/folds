import scala.annotation.tailrec

object Folding extends App {
  type Fold[ELEM, COMB]   = (Option[(ELEM, COMB)] => COMB) => List[ELEM] => COMB
  type UnFold[ELEM, COMB] = (COMB => Option[(ELEM, COMB)]) => COMB => List[ELEM]

  def foldLeft[ELEM, COMB](zero: COMB)(f: (COMB, ELEM) => COMB)(list: List[ELEM]): COMB =
    list match {
      case Nil          => zero
      case head :: tail => foldLeft(f(zero, head))(f)(tail)
    }

  def foldRight[ELEM, COMB](f: (ELEM, COMB) => COMB)(z: COMB)(list: List[ELEM]): COMB =
    list match {
      case Nil          => z
      case head :: tail => f(head, foldRight(f)(z)(tail))
    }

  def fold[ELEM, COMB](out: Option[(ELEM, COMB)] => COMB)(list: List[ELEM]): COMB =
    list match {
      case Nil          => out(None)
      case head :: tail => out(Some((head, fold(out)(tail))))
    }

  def cata[F[_], ELEM, COMB](process: (F[ELEM] => COMB) => F[ELEM] => COMB)(fa: F[ELEM]): COMB =
    process(cata(process))(fa)

  def unfold[ELEM, COMB](into: COMB => Option[(ELEM, COMB)])(seed: COMB): List[ELEM] =
    into(seed) match {
      case None               => Nil
      case Some((head, next)) => head :: unfold(into)(next)
    }

  def ana[F[_], ELEM, COMB](process: (COMB => F[ELEM]) => COMB => F[ELEM])(seed: COMB): F[ELEM] =
    process(ana(process))(seed)

  val add: Option[(Int, Int)] => Int = {
    case Some(a, b) => a + b
    case None => 0
  }

  sealed trait GList[+H, +T] extends Product with Serializable {
    type Inner
  }

  case object GNil extends GList[Nothing, Nothing]
  case class GCons[H, T](head: H, tail: T) extends GList[H, T]

  type ListF[H] = [T] =>> GList[H, T]
  //  type List[H] = GList[H, GList[H, List[?]]]

  trait StructureFunctor[F[_]] {
    extension[A](fa: F[A]) {
      def smap[B](f: A => B): F[B]
    }
  }

  object StructureFunctor {
    def apply[F[_]](using s: StructureFunctor[F]): StructureFunctor[F] = s
  }

  case class Fix[F[_]](unfix: F[Fix[F]])

  def toListF[H](lst: List[H]): Fix[ListF[H]] = lst match {
    case Nil => Fix(GNil)
    case h :: t => Fix(GCons(h, toListF(t)))
  }

  given [H]: StructureFunctor[ListF[H]] with {
    extension[T](fa: ListF[H][T]) {
      def smap[B](f: T => B): ListF[H][B] = fa match {
        case GNil => GNil
        case GCons(h, t) => GCons(h, f(t))
      }
    }
  }

  def cataF[F[_]: StructureFunctor, A](process: F[A] => A)(fa: Fix[F]): A =
    process(fa.unfix.smap(cataF(process)))

  def addCata(next: List[Int] => Int): List[Int] => Int = {
    case Nil    => 0
    case h :: t => h + next(t)
  }

  def mapCata[A, B](f: A => B)(next: List[A] => List[B]): List[A] => List[B] = {
    case Nil    => Nil
    case h :: t => f(h) :: next(t)
  }

  def idCata[A](next: List[A] => List[A]): List[A] => List[A] = {
    case Nil    => Nil
    case h :: t => h :: next(t)
  }

  val foldRes = cata(addCata)(List(1, 2, 3))

  val upto10: Int => Option[(Int, Int)] = { seed =>
    if (seed < 10) Some((seed, seed + 1))
    else None
  }

  def upto10Ana(next: Int => List[Int]): Int => List[Int] = { seed =>
    if (seed < 10) seed :: next(seed + 1)
    else Nil
  }

  val unfoldRes = ana(upto10Ana)(0)

  val m: GList[Int, Int] => Int = {
    case GNil        => 0
    case GCons(h, t) => h + t
  }

  val convert: Fix[ListF[Int]] = toListF(List(1, 2, 3))

  println(convert)
}
