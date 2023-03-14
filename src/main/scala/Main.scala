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

  def paresord(lst: List[(Int, Int)]): List[(Int, Int)] = lst match {
    case Nil => Nil
    case h :: t => if (h._1 < h._2) h :: paresord(t) else
      paresord(t)
  }

  def paresord2(lst: List[(Int, Int)]): List[(Int, Int)] = lst filter (x => x._1 < x._2)

  //  b) myconcat function that receives a list of strings and joins them(concatenated) in
  //  a single string.

  def myconcat(lst: List[String]): String = lst match {
    case Nil => ""
    case h :: t => h + myconcat(t)
  }

  def myconcat2(lst: List[String]): String = (lst foldLeft "")(_ ++ _)

  //  c) maximum function that receives a list of pairs of numbers (of type Double) and calculates
  //  a list with just the largest element of each pair.
  //  Example: List((2.0, 4.0), (3.2, 1, 9)) => List(4.0, 3.2)

  def maximum(lst: List[(Double, Double)]): List[Double] = lst match {
    case Nil => Nil
    case h :: t => if (h._1 > h._2) h._1 :: maximum(t)
    else h._2 :: maximum(t)
  }

  def maximum2(lst: List[(Double, Double)]): List[Double] = {
    lst map (x => if (x._1 > x._2) x._1 else x._2)
  }

//  Exercise 5

//  Evaluate manually each of the following expressions before executing them.
//  a) List(1, 2, 3, 4, 5) map (x => x % 2 != 0) // List(true, false, true, false, true)
//  b) List(1, 2, 3, 4, 5) filter (x => x % 2 != 0) // List(1, 3, 5)
//  c) List(5, 6, 23, 3) map (x => x % 3 == 0) // List(false, true, false, true)
//  d) List(5, 6, 23, 3) filter (x => x % 3 == 0) // List(6, 3)
//  e) List(1, 3, 7, 8, 12, 15) filter (x => x < 7) // List(1, 3)
//  f) List(List(2, 3), List(1, 5, 3)) map (x => 7 :: x) // List(List(7, 2, 3), List(7, 1, 5, 3))
//  g) List(1, 2, 3, 4, 5) map (x => List(x)) // List(List(1), List(2), List(3), List(4), List(5))
//  h) List(1, 2, 3, 4, 5) filter (x => x % 2 != 0) map (x => x + 1) // List(2, 4, 6)
//  i) List(1, 2, 3, 4, 5) map (x => x + 1) filter (x => x % 2 != 0) // List(3, 5)

  def main(args: Array[String]): Unit = {
    val lst = List((2.0, 4.0), (3.2, 1.9), (20.3, 4.0), (3.2, 10.9))
    val lst1 = List("Alex", "Is", "Someone", "Who", "Likes", "To", "Code")
    val lst2 = List((2, 4), (3, 1), (20, 4), (3, 10), (11, 12))
//    println(paresord2(lst2))
    println(List(1, 2, 3, 4, 5) map (x => x + 1) filter (x => x % 2 != 0))
  }
}