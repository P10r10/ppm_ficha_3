import scala.annotation.tailrec

object Main {

  //  1.1.Define three versions of the factorial method:

  //    a) without using an if

  def factA(x: Int): Int = x match {
    case 0 => 1
    case y => y * factA(x - 1)
  }

  //    b) using an if

  def factB(x: Int): Int = {
    if (x == 0) 1
    else x * factB(x - 1)
  }

  //    c) tail recursive

  def factC(x: Int) = {
    @tailrec
    def aux(acc: Int, y: Int): Int = y match {
      case 0 => acc
      case y => aux(acc * y, y - 1)
    }

    aux(1, x)
  }

  //  1.2.Define two versions of the remDup polymorphic / generic method that eliminates
  //  consecutive duplicates of a list of elements. Use the dropWhile function.

  //  a) standard recursive

  def remDupA[A](lst: List[A]): List[A] = lst match {
    case Nil => Nil
    case h :: t => h :: remDupA(t.dropWhile(_ == h))
  }

  //  b) tail recursive

  def remDupB[A](lst: List[A]): List[A] = {
    @tailrec
    def aux(acc_lst: List[A], lst1: List[A]): List[A] = lst1 match {
      case Nil => acc_lst
      case h :: t => aux(acc_lst :+ h, t.dropWhile(_ == h))
    }

    aux(Nil, lst)
  }

  //  Exercise 3

  //  a) Write a function that accepts two lists(with the same length) and constructs a
  //  new list by adding corresponding elements.
  //  For example, List(1, 2, 3) and List(4, 5, 6) becomes List(5, 7, 9).

  def addLists(lst1: List[Int], lst2: List[Int]): List[Int] = (lst1, lst2) match {
    case (Nil, Nil) => Nil
    case (h1 :: t1, h2 :: t2) => h1 + h2 :: addLists(t1, t2)
  }

  //  b) Generalize the function you just wrote so that it is not specific to integers or addition.
  //  Name your generalized function zipWith.

  def zipWith[A](f: (A, A) => A)(lst1: List[A], lst2: List[A]): List[A] = (lst1, lst2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (h1 :: t1, h2 :: t2) => f(h1, h2) :: zipWith(f)(t1, t2)
  }

  //  c) Implement isSorted, which checks whether a List[A] is sorted according to a given
  //  comparison function:

  def isSorted[A](lst: List[A], ordered: (A, A) => Boolean): Boolean = lst match {
    case Nil => true
    case _ :: Nil => true
    case x :: y :: tail => (ordered(x, y)) && isSorted(y :: tail, ordered)
  }

//  d) Implement bubbleSort (recursive) that can be used for ascending / descending ordering.

//  def bubbleSort(data: List[Int], f: (Int, Int) => Boolean): List[Int] = //TODO

//  Exercise 4

//  Define the following three new functions and say if they match any of the patterns
//  (mapping, filtering, folding)

//  a) paresord function that receives a list of pairs of numbers and returns only the pairs
//  in that the first component is inferior to the second.

  def paresord(lst: List[(Int, Int)]):List[(Int, Int)] = lst match {
    case Nil => Nil
    case h :: t => if (h._1 < h._2) h :: paresord(t) else
                  paresord(t)
  }

  def main(args: Array[String]): Unit = {
    val lst = List((1,2), (4,3), (5,10), (3,1), (9,11))
    println(paresord(lst))
  }
}