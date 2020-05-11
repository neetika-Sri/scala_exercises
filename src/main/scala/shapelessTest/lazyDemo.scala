package shapelessTest

import shapeless.Lazy

object lazyDemo {
  sealed trait List[+T]
  case class Cons[T](hd: T, tl: List[T]) extends List[T]

  sealed trait Nil extends List[Nothing]
  case object Nil extends Nil

  trait Show[T] {
    def apply(t: T): String
  }

  object Show {
    implicit def showInt: Show[Int] = new Show[Int]{
      def apply(t: Int) = t.toString
    }

    implicit def showNil: Show[Nil] = new Show[Nil]{
      def apply(t:Nil) = "Nil"
    }

    implicit def showCons[T](implicit st:Lazy[Show[T]], sl: Lazy[Show[List[T]]]): Show[Cons[T]] = new Show[Cons[T]]{
      def apply(t: Cons[T]) = s"Cons(${show(t.hd)(st.value)},${show(t.tl)(sl.value)})"
    }

    implicit def showList[T](implicit sc:Lazy[Show[Cons[T]]]): Show[List[T]] = new Show[List[T]]{
      def apply(t: List[T]) = t match {
        case n:Nil => show(n);
        case c:Cons[T]=> show(c)(sc.value)
      }
    }
  }
  def show[T](t:T)(implicit s:Show[T]) = s(t)


}
