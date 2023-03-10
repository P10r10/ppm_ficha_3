import scala.annotation.tailrec

object Main {

//  1.1.Define three versions of the factorial method:

//    a) without using an if

  def fact_a(x: Int): Int = x match {
    case 0 => 1
    case y => y * fact_a(x - 1)
  }

//    b) using an if

  def fact_b(x: Int): Int = {
    if (x == 0) 1
    else x * fact_b(x - 1)
  }

//    c) tail recursive

  def fact_c(x: Int) = {
    @tailrec
    def aux(acc: Int, y: Int): Int = y match {
      case 0 => acc
      case y => aux(acc * y, y - 1)
    }
    aux(1, x)
  }


//  1.2.Define two versions of the remDup polymorphic / generic method that eliminates
//    consecutive duplicates of a list of elements. Use the dropWhile function.

//  a) standard recursive

  def remDup_a[A](lst: List[A]): List[A] = lst match {
    case Nil => Nil
    case h :: t => h :: remDup_a(t.dropWhile(_ == h))
  }

//  b) tail recursive

  def remDup_b[A](lst: List[A]): List[A] = {
    @tailrec
    def aux(acc_lst: List[A], lst1: List[A]):List[A] = lst1 match {
      case Nil => acc_lst
      case h :: t => aux(acc_lst :+ h, t.dropWhile(_ == h))
    }
    aux(Nil, lst)
  }

//  Exercise 3

//  a) Write a function that accepts two lists(with the same length) and constructs a
//  new list by adding corresponding elements.
//  For example, List(1, 2, 3) and List(4, 5, 6) becomes List(5, 7, 9).



  def main(args: Array[String]): Unit = {
    val lst = List('a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e')
    println(remDup_b(lst))
  }
}