import scala.annotation.tailrec
import scala.collection.mutable

enum MyList[+A]:
  case MyNil
  case MyCons(hd: A, tl: MyList[A])

  override def toString: String =
    @scala.annotation.tailrec
    def go(sb: StringBuilder, as: MyList[A]): String = {
      as match {
        case MyNil =>
          sb.result
        case MyCons(h, t) =>
          go(
            sb
              .append(h)
              .append(if t == MyNil then "]" else ", "),
            t
          )
      }
    }
    go(new StringBuilder("["), this)

object MyList:
  def apply[A](xs: A*): MyList[A] = of(xs*)
  def of[A](xs: A*): MyList[A] =
    xs.foldRight(MyNil: MyList[A]) { case (x, acc) => MyCons(x, acc) }

import MyList.*

@tailrec
def dropWhile[A](xs: MyList[A], pred: A => Boolean): MyList[A] =
  xs match
    case MyNil => MyNil
    case MyCons(hd, tl) =>
      if (pred(hd))
        dropWhile(tl, pred)
      else
        xs

def takeWhile[A](xs: MyList[A], pred: A => Boolean): MyList[A] =
  xs match
    case MyNil => MyNil
    case MyCons(hd, tl) =>
      if (pred(hd))
        MyCons(hd, takeWhile(tl, pred))
      else
        MyNil

def subsequences[A](xs: MyList[A]): MyList[MyList[A]] =
  def insertToEach[A](a: A, xs: MyList[MyList[A]]): MyList[MyList[A]] =
    xs match
      case MyNil => MyNil
      case MyCons(hd, tl) => MyCons(MyCons(a, hd), insertToEach(a, tl))
  def concat[A](xs: MyList[A], ys: MyList[A]): MyList[A] =
    xs match
      case MyNil => ys
      case MyCons(hd, tl) => MyCons(hd, concat(tl, ys))
  def getSubWithLength[A](xs: MyList[A], l: Int): MyList[MyList[A]] =
    if (l > 0)
      xs match
        case MyNil => MyNil
        case MyCons(hd, tl) => concat(insertToEach(hd, getSubWithLength(tl, l - 1)), getSubWithLength(tl, l))
    else
      MyCons(MyNil, MyNil)
  def length[A](xs: MyList[A]): Int =
    @tailrec
    def go(xs: MyList[A], res: Int): Int =
      xs match
        case MyNil => res
        case MyCons(_, tl) => go(tl, res + 1)
    go(xs, 0)
  @tailrec
  def go(l: Int, res: MyList[MyList[A]]): MyList[MyList[A]] =
    if (l >= 0)
      go(l - 1, concat(getSubWithLength(xs, l), res))
    else
      res
  go(length(xs) - 1, MyCons(xs, MyNil))

@tailrec
def corresponds[A, B](as: MyList[A], bs: MyList[B], f: (A, B) => Boolean): Boolean =
  (as, bs) match
    case (MyNil, MyNil) => true
    case (MyNil, MyCons(_, _)) => false
    case (MyCons(_, _), MyNil) => false
    case (MyCons(h1, t1), MyCons(h2, t2)) =>
      if (f(h1, h2))
        corresponds(t1, t2, f)
      else
        false

@main def run(): Unit =
  println("Hello world")
