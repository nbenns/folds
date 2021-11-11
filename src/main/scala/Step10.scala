import scala.annotation.tailrec

object Step10 extends App {
  enum GList[+H, +T] {
    case Nil extends GList[Nothing, Nothing]
    case Cons(head: H, tail: T) extends GList[H, T]
  }

  type ListF[H] = [T] =>> GList[H, T]
  //  type List[H] = GList[H, GList[H, List[?]]]

  trait Functor[F[_]] {
    extension[A](fa: F[A]) {
      def map[B](f: A => B): F[B]
    }
  }

  object Functor {
    def apply[F[_]](using s: Functor[F]): Functor[F] = s
  }

  case class Fix[F[_]](unfix: F[Fix[F]])

  def toListF[H](lst: List[H]): Fix[ListF[H]] = lst match {
    case Nil => Fix(GList.Nil)
    case h :: t => Fix(GList.Cons(h, toListF(t)))
  }

  given [H]: Functor[ListF[H]] with {
    extension[T](fa: ListF[H][T]) {
      def map[B](f: T => B): ListF[H][B] = fa match {
        case GList.Nil => GList.Nil
        case GList.Cons(h, t) => GList.Cons(h, f(t))
      }
    }
  }

  def cataF[F[_]: Functor, A](process: F[A] => A)(fa: Fix[F]): A =
    process(fa.unfix.map(cataF(process)))

  val m: GList[Int, Int] => Int = {
    case GList.Nil        => 0
    case GList.Cons(h, t) => h + t
  }

  val convert: Fix[ListF[Int]] = toListF(List(1, 2, 3))

  println(convert)
}
