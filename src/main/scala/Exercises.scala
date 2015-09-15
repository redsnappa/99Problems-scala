import scala.util.Random

/**
 * Created by dan on 04/09/2015.
 */
object P01{

  /**
   * P01 - Finds the last element of a list
   * @param list
   * @tparam T
   * @return T
   */
  def last[T](list:List[T]):T = list match {
    case List() => throw new Exception("No last in empty")
    case x::Nil => x
    case x::xs => last(xs)
  }


}

object P02 {
  /**
   *
   * P02 (*) Find the last but one element of a list.
   * Example:
   * scala> penultimate(List(1, 1, 2, 3, 5, 8))
   * res0: Int = 5
   *
   * @param list
   * @tparam T
   * @return T
   *
   */

  def penultimate[T](list: List[T]): T = list match {
    case List() => throw new Exception("Can't get penultimate of empty list")
    case x :: Nil => throw new Exception("Can't get penultimate of list of length one")
    case x :: y :: Nil => x
    case x :: xs => penultimate(xs)
  }

}

object P03{

    /**
     * P03 (*) Find the Kth element of a list.
     * By convention, the first element in the list is element 0.
     * Example:
     *   scala> nth(2, List(1, 1, 2, 3, 5, 8))
     *   res0: Int = 2
     *
     * @param n : the index to retrieve
     * @param list
     * @tparam T
     * @return T
     *
     */

    def nth[T](n:Int,list:List[T]): T = {

      def innerNth(count:Int, innerList:List[T]):T = {
        if(count == n){
          innerList.head
        }else{
          innerNth(count + 1, innerList.tail)
        }
      }

      if(n < list.length){
        innerNth(0,list)
      }else{
        throw new IllegalArgumentException("Param n was larger than length of the list")
      }
    }


  //solution gives a good implementation
  def Nnth[T](n:Int,list:List[T]):T =  (n,list) match {

    case (_,List()) => throw new IllegalArgumentException("No nth of empty list")
    case (0, x::xs) =>  x
    case (_, y::ys) => Nnth(n-1, ys)

  }
}

object P04{

  /**
   * P04 (*) Find the number of elements of a list.
   * Example:
   * scala> length(List(1, 1, 2, 3, 5, 8))
   * res0: Int = 6
   *
   * @param list
   * @return Int: length of the list
   */

  def length[T](list:List[T]):Int = {
    (list foldLeft(0))((x,_) => x + 1)
  }

}


object P05{

  /**
   * P05 (*) Reverse a list.
   * Example:
   * scala> reverse(List(1, 1, 2, 3, 5, 8))
   * res0: List[Int] = List(8, 5, 3, 2, 1, 1)
   *
   * @param list
   * @tparam T
   * @return list - reversed list
   *
   */

  def reverse[T](list:List[T]):List[T] = {
    list.foldLeft(List[T]()) ((lst, elem) => elem :: lst )
  }

}

object P06{

  /**
   *  P06 (*) Find out whether a list is a palindrome.
   *  Example:
   *  scala> isPalindrome(List(1, 2, 3, 2, 1))
   *  res0: Boolean = true
   *
   *  @param list
   *  @tparam T
   *  @return boolean
   */

  def isPalindrome[T](list:List[T]):Boolean = {

    def innerPalin(inlist:List[T],revList:List[T]):Boolean = {
      (inlist.isEmpty && revList.isEmpty)  || ((revList.head == inlist.head) && (innerPalin(inlist.tail,revList.tail)))

    }
    innerPalin(list,list.reverse)
  }
}

object P07{

  /**
   * P07 (**) Flatten a nested list structure.
   * Example:
   * scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
   * res0: List[Any] = List(1, 1, 2, 3, 5, 8)
   *
   *  @param list
   *  @return List[Any]
   */

  def flatten(list:List[Any]):List[Any] = {

    //use flatMap
    list flatMap {
      case ms:List[_] => flatten(ms)
      case e => List(e)
    }

  }
}


object P08{

  /**
   *
   * P08 (**) Eliminate consecutive duplicates of list elements.
   * If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
   *
   * Example:
   *
   * scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   * res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
   *
   *
   *
   */

  def compress[T](list:List[T]):List[T] = {

    list.foldRight (List[T]()) ((c,l) => if(!l.isEmpty && c==l.head) l else c ::l)

  }
}

object P09{

  /**
   *  P09 (**) Pack consecutive duplicates of list elements into sublists.
   * If a list contains repeated elements they should be placed in separate sublists.
   * Example:
   *    scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   *    res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
   */

  def pack[T](list:List[T]):List[List[T]] = {

    (list foldRight(List[List[T]]())) (
      (x,listOfList) =>  if(listOfList.isEmpty || x != listOfList.head.head) List[T](x) :: listOfList else  (x :: listOfList.head) :: listOfList.tail
    )

  }


}

object P10{

  /**
   *
   * P10 (*) Run-length encoding of a list.
   * Use the result of problem P09 to implement the so-called run-length encoding data compression method.
   * Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
   *
   * Example:
   *
   * scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   * res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
   *
   */

  def encode[T](list:List[T]):List[(Int,T)] = {

    P09.pack(list) map { xs => (xs.length,xs.head) }

  }

}

object P11{

  /**
   *
   * P11 (*) Modified run-length encoding.
   * Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list.
   * Only elements with duplicates are transferred as (N, E) terms.
   *
   * Example:
   *
   * scala> encodeModified(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))
   * res0: List[Any] = List((4,'a'), 'b', (2,'c'), (2,'a'), 'd', (4,'e'))
   *
   */

  def encodeModified[T](list:List[T]):List[Any] = {

    P10.encode(list) map { x=> if(x._1 == 1) x._2 else x  }

  }

}


object P12{

  /**
   *
   * P12 (**) Decode a run-length encoded list.
   * Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
   * Example:
   *
   * scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
   * res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
   *
   */

  def decode[T](list:List[(Int,T)]): List[T] = {

    list flatMap ((elem) => List.fill(elem._1)(elem._2))

  }

}

object P13{

  /**
   *  P13 (**) Run-length encoding of a list (direct solution).
   * Implement the so-called run-length encoding data compression method directly. I.e. don't use other methods you've written (like P09's pack); do all the work directly.
   * Example:
   *
   * scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   * res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
   *
   */

  def encodeDirect[T](list:List[T]):List[(Int,T)] = {

    def enc(innerList: List[T], result: List[(Int, T)]): List[(Int, T)] = {

      val (next,rest) = innerList.span(elem => elem == innerList.head)

      (innerList, result) match {
        case (Nil, _) => result
        case (x :: xs, _) => enc(rest, (next.length, x) :: result)

      }
    }

    enc(list,Nil).reverse

  }
}

object P14{


  /**
    * P14 (*) Duplicate the elements of a list.
    * Example:
    * scala> duplicate(List('a, 'b, 'c, 'c, 'd))
    * res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
    *
    */


  def duplicate[T](list:List[T]):List[T] = {

    list flatMap(elem => List.fill(2)(elem))

  }

}


object P15{

  /**
   * P15 (**) Duplicate the elements of a list a given number of times.
   * Example:
   * scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
   * res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
   *
   */

    def duplicateN[T](n:Int,list:List[T]):List[T] =  list flatMap(elem => List.fill(n)(elem))


}

object P16{

  /**
   *  P16 (**) Drop every Nth element from a list.
   * Example:
   * scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   * res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
   *
   */

  def drop[T](i:Int,list:List[T]):List[T] = {

    if(i>0){
      ((list.zipWithIndex filter (elem => ((elem._2+1) % i) != 0)).unzip)._1
    }else{
      list
    }


  }
}

object P17{

  /**
   *  P17 (*) Split a list into two parts.
   * The length of the first part is given. Use a Tuple for your result.
   * Example:
   *
   * scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   * res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   *
   */

  def split[T](i:Int,list:List[T]):(List[T],List[T]) = {

    (list take(i) , list drop(i))

  }
}




object P18{

  /**
   *
   * P18 (**) Extract a slice from a list.
   * Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list. Start counting the elements with 0.
   * Example:
   *
   * scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   * res0: List[Symbol] = List('d, 'e, 'f, 'g)
   *
   */

  def slice[T](start:Int, end:Int, list:List[T]):List[T] = {

    if(end < start) throw new IllegalArgumentException("Start must be more than or equal to end")
    val tailCut = if(start> 0) (end - start) else end
    (list drop start) take tailCut

  }

}

object P19{

  /**
   *
   * P19 (**) Rotate a list N places to the left.
   * Examples:
   * scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   * res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
   *
   * scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   * res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
   *
   */

  def rotate[T](i:Int,list:List[T]):List[T] = {

    if(i>=0){
      (list drop i) ::: (list take i)

    }else{
      (list takeRight(Math.abs(i))) :::
      (list dropRight(Math.abs(i)))
    }
  }

}


object P20{

  /**
   * P20 (*) Remove the Kth element from a list.
   * Return the list and the removed element in a Tuple. Elements are numbered from 0.
   * Example:
   *
   * scala> removeAt(1, List('a, 'b, 'c, 'd))
   * res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
   *
   */

  def removeAt[T](idx:Int, list:List[T]):List[T] = {

    if(idx < 0 ) throw new IllegalArgumentException("Can't remove item at " + idx)
    (list take (idx)) ::: (list drop(idx+1))

  }

}

object P21{

  /**
   *
   *    P21 (*) Insert an element at a given position into a list.
   *    Example:
   *    scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
   *    res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
   *
   */

  def insertAt[T](newItem:T, idx:Int, list:List[T]): List[T] = {

    list.splitAt(idx) match {

      case (_,Nil) if idx > list.length =>   throw new IllegalArgumentException("Can't add new item beyond the length of the list: index [" + idx + "] / length of list [" +   list.length + "]" )
      case (Nil,_) if idx < 0 => throw new IllegalArgumentException("Can't add new item to a negative index: [" + idx + "]")
      case (pre,post) => pre ::: (newItem :: post)

    }
  }

}

object P22{

  /**
   *
   * P22 (*) Create a list containing all integers within a given range.
   *  Example:
   *   scala> range(4, 9)
   *   res0: List[Int] = List(4, 5, 6, 7, 8, 9)
   *
   */

  def range(start:Int,limit:Int):List[Int] = {

    def tailRange(num:Int,result:List[Int]):List[Int] = {

      if(num < start){
        result
      } else{
        tailRange(num-1,num :: result)
      }
    }

    if(start > limit) throw new IllegalArgumentException("Start must be less than or equal to the limit")
    else{
      tailRange(limit,Nil)
    }

  }

}

object P23{

  /**
   *
   * P23 (**) Extract a given number of randomly selected elements from a list.
   *  Example:
   *   scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
   *   res0: List[Symbol] = List('e, 'd, 'a)
   *   Hint: Use the solution to problem P20
   *
   */

  def randomSelect[T](num:Int,list:List[T]):List[T] = {

    //TODO creating a Random is expensive, we should create it once and pass it for use in the inner function!!
    def innerRS(leftToRemove: Int, result: List[T]): List[T] = leftToRemove match {

      case 0 => result
      case _ => innerRS(leftToRemove - 1, P20.removeAt(new Random().nextInt(result.length), result))

    }

    if(num < 0 ) throw new IllegalArgumentException("Can't extract a negative amount of numbers")
    else if(list.length < num) throw new IllegalArgumentException("Can't extract a larger number of items than items in the inital list")
    else innerRS(list.length-num,list)
  }

}

object P24{

  /**
   *
   *  P24 (*) Lotto: Draw N different random numbers from the set 1..M.
   *   Example:
   *   scala> lotto(6, 49)
   *   res0: List[Int] = List(23, 1, 17, 33, 21, 37)
   */

  def lotto(numberOfItems:Int,M:Int):List[Int] = {
    import P23.randomSelect
    import P22.range
    if(numberOfItems < 0) throw new IllegalArgumentException("Cannot generate a List of negative size")
    else if(M < 0) throw new IllegalArgumentException("Parameter M must be positive, cannot generate negative range for lotto")
    randomSelect(numberOfItems, range(1,M))

  }

}

object P25{

  /**
   *  P25 (*) Generate a random permutation of the elements of a list.
   *   Hint: Use the solution of problem P23.
   *   Example:
   *      scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
   *      res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)
   */

  def randomPermute[T](list:List[T]):List[T] = {

    val random = new Random()

    def perm(ls:List[T], result:List[T]): List[T] = {

      val randomIndex = if(ls.length > 0) random.nextInt(ls.length) else 0

      ls match {

        case Nil => result
        case x::xs => perm(P20.removeAt(randomIndex,ls),ls(randomIndex) :: result)

      }

    }
    perm(list,Nil)

  }

}




