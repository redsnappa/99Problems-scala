import org.scalatest.FlatSpec

/**
 * Created by dan on 04/09/2015.
 */
class P01Test extends FlatSpec {

  /**
   * P01 Tests
   */

   "P01- given an empty list" should "throw Exception with message" in {
    val thrown = intercept[Exception] {
      P01.last(List())
    }
     assert(thrown.getMessage === "No last in empty")
  }


  "P01- given a list of one" should "return the element" in {

    val one =  P01.last(List(1))

    assert(one === 1)
  }

  "P01- given a list" should "return the last element" in {

    val five =  P01.last(List(1,2,3,4,5))

    assert(five === 5)
  }

}

class P02Test extends FlatSpec {

  /**
   * P02 Tests
   */

  "P02- given an empty list" should "return an empty list" in {

    val thrown = intercept[Exception] {
      P02.penultimate(List())
    }
    assert(thrown.getMessage === "Can't get penultimate of empty list")


  }

  "P02- given a list of one element" should "throw exception" in {


    val thrown = intercept[Exception] {
      P02.penultimate(List(1))
    }
    assert(thrown.getMessage === "Can't get penultimate of list of length one")

  }

  "P02- given a list of two elements" should "return the first element" in {

    val result = P02.penultimate(List(1,2))
    assert(result === 1)

  }

  "P02- given a list of multiple elements" should "return the  penultimate element" in {

    val result = P02.penultimate(List(1, 1, 2, 3, 5, 8))
    assert(result === 5)

  }

}

class P03Test extends FlatSpec {

  /**
   * P03 Tests
   */

  "P03- given an empty list" should "throw an exception" in {

    val thrown = intercept[IllegalArgumentException]{

      P03.nth(3, List())

    }
    assert(thrown.getMessage == "Param n was larger than length of the list")

  }

  "P03 - given a 0 and list " should "return the value at the head of that list" in {

    val result = P03.nth(0, List(3, 1, 2, 3, 5, 8))
    assert(result === 3)
  }

  "P03 - given a value and list " should "return the value at the index of that list" in {

    val result = P03.nth(2, List(1, 1, 2, 3, 5, 8))
    assert(result === 2)
  }

  "P03solution given an empty list" should "throw an exception" in {

    val thrown = intercept[IllegalArgumentException]{

      P03.Nnth(3, List())

    }
    assert(thrown.getMessage == "No nth of empty list")

  }

  "P03solution - given a 0 and list " should "return the value at the head of that list" in {

    val result = P03.Nnth(0, List(3, 1, 2, 3, 5, 8))
    assert(result === 3)
  }

  "P03solution - given a value and list " should "return the value at the index of that list" in {

    val result = P03.Nnth(2, List(1, 1, 2, 3, 5, 8))
    assert(result === 2)
  }

}

class P04Test extends FlatSpec {

  /**
   * P04 Tests
   */

  "P04 - give an empty list" should "return 0" in {
    assert(0 === P04.length(List()))

  }

  "P04 - give a list" should "return the length" in {
    assert(5 === P04.length(List(1,2,3,4,5)))

  }

}

class P05Test extends FlatSpec {

  /**
   * P05 Tests
   */

  "P05 - given an empty list to reverse" should "return an empty list" in {

    assert(List() === P05.reverse(List()))

  }

  "P05 - given a list of one to reverse" should "return the same list" in {

    assert(List(3) === P05.reverse(List(3)))

  }

  "P05 given a list to reverse" should "return the revese" in {

    assert(List(5,4,3,2,1,0,-1) === P05.reverse(List(-1,0,1,2,3,4,5)))

  }

}

class P06Test extends FlatSpec{

  /**
   * P06 Tests
   */

  "P06 - given an empty list" should "be a palindrome" in {

    assert(P06.isPalindrome(List()))

  }

  "P06 - given a list of one" should "be a palindrome" in {

    assert(P06.isPalindrome(List('a')))

  }

  "P06 - given the string 'abba'" should "be a palindrome" in {

    assert(P06.isPalindrome("abba".toList))

  }

  "P06 - given the string 'abbaa'" should "not be a palindrome" in {

    assert(!P06.isPalindrome("abbaa".toList))

  }

  "P06 - given the List(1,2,3,2,1)'" should " be a palindrome" in {

    assert(P06.isPalindrome(List(1, 2, 3, 2, 1)))

  }
}


class P07Test extends FlatSpec {

  "P07 - given the List(List(1, 1), 2, List(3, List(5, 8)))" should " return List(1,1,2,3,5,8)" in {

    assert(List(1,1,2,3,5,8) === P07.flatten(List(List(1, 1), 2, List(3, List(5, 8)))))

  }

}

class P08Test extends FlatSpec{


  "P08 given an empty list to compress" should "return an empty list" in {

    assert(List()===P08.compress(List()))

  }

  "P08 given List('a')" should "return a List('a')" in {

    assert(List('a')===P08.compress(List('a')))

  }

  "P08 given List('a','a')" should "return a List('a')" in {

    assert(List('a')===P08.compress(List('a','a')))

  }


  "P08 given  List('a','a','b')" should "return a List('a','b')" in {

    assert(List('a','b')===P08.compress(List('a','a','b')))

  }


  "P08 given List('a','a','a','b','b')" should "return a List('a','b')" in {

    assert(List('a','b')===P08.compress(List('a','a','a','b','b')))

  }


  "P08 given List('a','b','b','a','a','b','b')" should "return a List('a','b')" in {

    assert(List('a','b','a','b')===P08.compress(List('a','b','b','a','a','b','b')))

  }

  "P08 given List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')" should "return a List('a', 'b', 'c', 'a', 'd', 'e')" in {

    assert(List('a', 'b', 'c', 'a', 'd', 'e')===P08.compress(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')))

  }

}

class P09Test extends FlatSpec {


  "P09 given List(1)" should "return List(List(1))" in {

    assert(List(List(1)) === P09.pack(List(1)))

  }

  "P09 given List(1,2,2,33,78,78)" should "return List(List(1).List(),)" in {

    assert(List(List(1)) === P09.pack(List(1)))

  }

  "P09 given List('a', 'a', 'a', 'a')" should "return List(List('a'))" in {

    assert(List(List('a','a','a','a')) === P09.pack(List('a','a','a','a')))

  }


  "P09 given List('a', 'a', 'a', 'b','b','c','d','d')" should "return List(List('a','a','a'),List('b','b'),List('c'),List('d','d'))" in {

    assert(List(List('a','a','a'),List('b','b'),List('c'),List('d','d')) === P09.pack(List('a', 'a', 'a', 'b','b','c','d','d')))

  }

  "P09 given List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')" should " return List(List('a', 'a', 'a', 'a'), List('b'), List('c', 'c'), List('a', 'a'), List('d'), List('e', 'e', 'e', 'e'))" in {

    assert(List(List('a', 'a', 'a', 'a'), List('b'), List('c', 'c'), List('a', 'a'), List('d'), List('e', 'e', 'e', 'e')) === P09.pack(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')))
 }

}


class P10Test extends FlatSpec {


  "P10 given List()" should "return List()" in {

    assert(List() === P10.encode(List()))

  }

  "P10 given List('a','b','c')" should "return List((1,'a'),(1,'b'),(1,'c'))" in {

    assert(List((1,'a'),(1,'b'),(1,'c')) === P10.encode(List('a','b','c')))

  }

  "P10 given List(1,1,1,2,1,1,1,2,2,2,2)" should "return List((3,1),(1,2),(3,1),(4,2))" in {

    assert(List((3,1),(1,2),(3,1),(4,2))=== P10.encode(List(1,1,1,2,1,1,1,2,2,2,2)))

  }


  "P10 given List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))" should "return List((4,'a'), (1,'b'), (2,'c'), (2,'a'), (1,'d'), (4,'e'))" in {

    assert(List((4,'a'), (1,'b'), (2,'c'), (2,'a'), (1,'d'), (4,'e')) === P10.encode(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')))

  }

}


class P11Test extends FlatSpec {


  "P11 given List()" should "return List()" in {

    assert(List() === P11.encodeModified(List()))

  }

  "P11 given List('a')" should "return List('a')" in {

    assert(List('a') === P11.encodeModified(List('a')))

  }


  "P11 given List(1,1,1,2,3,4,3,3,3)" should "return List((3,1),2,3,4,(3,3))" in {

    assert(List((3,1),2,3,4,(3,3)) === P11.encodeModified(List(1,1,1,2,3,4,3,3,3)))

  }


  "P11 given List('a','a','c','a','c','b','c')" should "return List((2,'a'),'c','a','c','b','c')" in {

    assert(List((2,'a'),'c','a','c','b','c') === P11.encodeModified(List('a','a','c','a','c','b','c')))

  }

  "P11 given List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')" should "return List((4,'a'), 'b', (2,'c'), (2,'a'), 'd', (4,'e'))" in {

    assert(List((4,'a'), 'b', (2,'c'), (2,'a'), 'd', (4,'e')) === P11.encodeModified(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')))


  }

}

class P12Test extends FlatSpec{


  "P12 given an List()" should "return List()"in {


    assert(List() === P12.decode(List()))

  }


  "P12 given List((1,1))" should "return List(1)"in {


    assert(List(1) === P12.decode(List((1,1))))

  }


  "P12 given List((1,1),(1,3),(2,3))" should "return List(1)"in {


    assert(List(1,3,3,3) === P12.decode(List((1,1),(1,3),(2,3))))

  }

  "P12 given List((4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')" should "return List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')" in {

    assert(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e') === P12.decode(List((4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e'))))

  }

}


class P13Test extends FlatSpec{

  "P13 given List()" should "return List()" in {

    assert(List() === P13.encodeDirect(List()))

  }

  "P13 given List('a','b','c')" should "return List((1,'a'),(1,'b'),(1,'c'))" in {

    assert(List((1,'a'),(1,'b'),(1,'c')) === P13.encodeDirect(List('a','b','c')))

  }

  "P13 given List(1,1,1,2,1,1,1,2,2,2,2)" should "return List((3,1),(1,2),(3,1),(4,2))" in {

    assert(List((3,1),(1,2),(3,1),(4,2))=== P13.encodeDirect(List(1,1,1,2,1,1,1,2,2,2,2)))

  }


  "P13 given List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))" should "return List((4,'a'), (1,'b'), (2,'c'), (2,'a'), (1,'d'), (4,'e'))" in {

    assert(List((4,'a'), (1,'b'), (2,'c'), (2,'a'), (1,'d'), (4,'e')) === P13.encodeDirect(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')))

  }


}


class P14Test extends FlatSpec {

  "P14 given List()" should "return List()" in {

    assert(List() === P14.duplicate(List()))

  }

  "P14 given List(1)" should "return List(1,1)" in {

    assert(List(1,1) === P14.duplicate(List(1)))

  }

  "P14 given List(1,1,2)" should "return List(1,1,1,1,2,2)" in {

    assert(List(1,1,1,1,2,2) === P14.duplicate(List(1,1,2)))

  }

  "P14 given List('a', 'b', 'c', 'c', 'd')" should "return List('a', 'a', 'b', 'b', 'c', 'c', 'c', 'c',  'd', 'd')" in {

    assert( List('a','a', 'b', 'b', 'c', 'c', 'c', 'c',  'd', 'd') === P14.duplicate(List('a', 'b', 'c', 'c', 'd')))

  }

}


class P15Test extends FlatSpec{



  "P15 given List()" should "return List()" in {

    assert(List() === P15.duplicateN(3,List()))

  }

  "P15 given 4, List(1)" should "return List(1,1,1,1)" in {

    assert(List(1,1,1,1) === P15.duplicateN(4,List(1)))

  }

  "P15 given 5,List(1,1,2)" should "return List(1,1,1,1,1,1,1,1,1,1,2,2,2,2,2)" in {

    assert(List(1,1,1,1,1,1,1,1,1,1,2,2,2,2,2) === P15.duplicateN(5,List(1,1,2)))

  }

  "P15 given List('a', 'b', 'c', 'c', 'd')" should "return List('a', 'a', 'a', 'b', 'b', 'b', 'c', 'c', 'c', 'c', 'c', 'c', 'd', 'd', 'd')" in {

    assert( List('a', 'a', 'a', 'b', 'b', 'b', 'c', 'c', 'c', 'c', 'c', 'c', 'd', 'd', 'd') === P15.duplicateN(3,List('a', 'b', 'c', 'c', 'd')))

  }

}

class P16Test extends FlatSpec {


  "P16 given 3 and List()" should "return List()" in {

    assert(List() === P16.drop(3,List()))

  }

  "P16 given 3 and List(1)" should "return List(1)" in {

    assert(List(1) === P16.drop(3,List(1)))

  }


  "P16 given 3 and List(1,2,3)" should "return List(1,2)" in {

    assert(List(1,2) === P16.drop(3,List(1,2,3)))
  }


  "P16 given 2 and List(1,2,3,4,5,6,7,8,9)" should "return List(1,3,5,7,9)" in {

    assert(List(1,3,5,7,9) === P16.drop(2,List(1,2,3,4,5,6,7,8,9)))
  }

  "P16 given 1 and List(1,2,3,4,5,6,7,8,9)" should "return List()" in {

    assert(List() === P16.drop(1,List(1,2,3,4,5,6,7,8,9)))
  }

  "P16 given 3 and List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')" should "return List('a', 'b', 'd', 'e', 'g', 'h', 'j', 'k')" in {

    assert(List('a', 'b', 'd', 'e', 'g', 'h', 'j', 'k') === P16.drop(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))
  }


  "P16 given -3 and List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')" should "return List('a', 'b', 'd', 'e', 'g', 'h', 'j', 'k')" in {

    assert(List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k') === P16.drop(-3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))
  }


  "P16 given 0 and List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')" should "return List('a', 'b', 'd', 'e', 'g', 'h', 'j', 'k')" in {

    assert(List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k') === P16.drop(0, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))
  }

}

class P17Test extends FlatSpec {

  "P17 given 3 and List()" should "return (List(),List())" in {

    assert((List(),List()) === P17.split(3,List()))

  }

  "P17 given 3 and List(1)" should "return (List(1),List())" in {

    assert((List(1),List()) === P17.split(3,List(1)))

  }

  "P17 given 3 and List(1,2,3)" should "return (List(1,2,3),List())" in {

    assert((List(1,2,3),List()) === P17.split(3,List(1,2,3)))

  }

  "P17 given 0 and List(1,2,3)" should "return (List(1,2,3),List())" in {

    assert((List(),List(1,2,3)) === P17.split(0,List(1,2,3)))

  }


  "P17 given 2 and List(1,2,3,4,5)" should "return (List(1,2),List(3,4,5))" in {

    assert((List(1,2),List(3,4,5)) === P17.split(2,List(1,2,3,4,5)))

  }

  "P17 given 3 and List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')" should "return (List('a', 'b', 'c'),List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))" in {

    assert((List('a', 'b', 'c'),List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k')) === P17.split(3,List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))

  }

}

class P18Test extends FlatSpec {

  "P18 given 0:start, 0:end and List('a','b','c')" should "return List()" in {

    assert(List() === P18.slice(0,0,List('a','b','c')))

  }

  "P18 given 1:start, 3:end and List()" should "return List()" in {

    assert(List() === P18.slice(1,3,List()))

  }

  "P18 given -1:start and 3:end with List(1,2,3,4,5)" should "return List(1,2)" in {

    assert(List(1,2,3)=== P18.slice(-1,3,List(1,2,3,4,5)))

  }


  "P18 given -1:start and 10:end with List(1,2,3,4,5)" should "return List(1,2,3,4,5)" in {

    assert(List(1,2,3,4,5)=== P18.slice(-1,10,List(1,2,3,4,5)))

  }


  "P18 given 2:start and 4:end with List(1,2,3,4,5)" should "return List(3,4)" in {
    assert(List(3,4)=== P18.slice(2,4,List(1,2,3,4,5)))
  }


  "P18 given 3:start and 7:end with List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')" should "return List('d', 'e', 'f', 'g')" in {
    assert(List('d', 'e', 'f', 'g')=== P18.slice(3,7,List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))
  }


  "P18 given 5:start and 1:end with List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')" should "throw an exception" in {

    val thrown = intercept[IllegalArgumentException]{
      P18.slice(5,1,List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))
    }

    assert("Start must be more than or equal to end" === thrown.getMessage)
  }

}

class P19Test extends FlatSpec{


  "P19 give 3 and List() " should "return List()" in {

    assert(List() === P19.rotate(3,List()))

  }

  "P19 give 0 and a list " should "return the same list" in {

    assert(List(1,2,3,4,5) === P19.rotate(0,List(1,2,3,4,5)))

  }


  "P19 rotate List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k') 3 places right" should "return List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'a', 'b', 'c')" in {

    assert(List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'a', 'b', 'c') === P19.rotate(3,List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))

  }


  "P19 rotate List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k') 2 places left" should "return List('j', 'k', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i'))" in {

    assert(List('j', 'k', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i') === P19.rotate(-2,List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))

  }

}


class P20Test extends FlatSpec {


  "P20 removeAt -1 from List('a', 'b', 'c', 'd')" should "throw IllegalArgument Exception" in {

    val thrown = intercept[IllegalArgumentException](
       P20.removeAt(-1 ,List('a', 'b', 'c', 'd')))


    assert("Can't remove item at -1" === thrown.getMessage)

  }

  "P20 removeAt 0 from List('a', 'b', 'c', 'd')" should "return List('b, 'c, 'd)" in {


    assert(List('b', 'c', 'd') === P20.removeAt(0,List('a', 'b', 'c', 'd')))

  }

  "P20 removeAt 1 from List('a', 'b', 'c', 'd')" should "return List('a, 'c, 'd)" in {


    assert(List('a', 'c', 'd') === P20.removeAt(1,List('a', 'b', 'c', 'd')))

  }

  "P20 removeAt 5 from List(0,1,2,3,4,5,6)" should "return List(0,1,2,3,4,5,6)" in {


    assert(List(0,1,2,3,4,6) === P20.removeAt(5,List(0,1,2,3,4,5,6)))

  }

  "P20 removeAt 10 from List('a', 'b', 'c', 'd'))" should "return List('a,'b', 'c, 'd')" in {

    assert(List('a','b','c', 'd') === P20.removeAt(10,List('a', 'b', 'c', 'd')))

  }

}

class P21Test extends FlatSpec {

  "P21 insertAt newItem: 'a', idx: 0, list: List('b','c','d')" should " return List('a, 'b','c','d')" in {

    assert(List('a', 'b','c','d') === P21.insertAt('a',0, List('b','c','d')))

  }

  "P21 insertAt newItem: 'e', idx: 4, list: List('a', 'b','c','d')" should " return List('a, 'b','c','d','e') with the new item at the end of the list" in {

    assert(List('a', 'b','c','d','e') === P21.insertAt('e',4, List('a','b','c','d')))

  }

  "P21 insertAt newItem: 'c', idx: 2, list: List('a','b','d')" should " return List('a, 'b','c','d')" in {

    assert(List('a', 'b','c','d') === P21.insertAt('c',2, List('a','b','d')))

  }

  "P21 insertAt newItem: 'z', idx: -1, list: List('a','b','c')" should " throw IllegalArgumentException" in {

    val thrown = intercept[IllegalArgumentException](
       P21.insertAt('z',-1, List('a','b','c'))
    )
    assert("Can't add new item to a negative index: [-1]" === thrown.getMessage)

  }


  "P21 insertAt newItem: 'z', idx: 10, list: List('a','b','c')" should " throw IllegalArgumentException" in {

    val thrown = intercept[IllegalArgumentException](
      P21.insertAt('z',10, List('a','b','c'))
    )
    assert("Can't add new item beyond the length of the list: index [10] / length of list [3]" === thrown.getMessage)

  }

  "P21 insertAt newItem: 't', idx: 0, list: List()" should " return List('t')" in {

    assert(List('t') === P21.insertAt('t',0, List()))

  }

  "P21 insertAt newItem: 't', idx: 1, list: List()" should " throw IllegalArgumentException" in {

    val thrown = intercept[IllegalArgumentException](
      P21.insertAt('t',1, List())
    )
    assert("Can't add new item beyond the length of the list: index [1] / length of list [0]" === thrown.getMessage)

  }

  "P21 insertAt newItem: 't', idx: 2, list: List()" should " throw IllegalArgumentException" in {

    val thrown = intercept[IllegalArgumentException](
      P21.insertAt('t',2, List())
    )
    assert("Can't add new item beyond the length of the list: index [2] / length of list [0]" === thrown.getMessage)
  }

}

class P22Test extends FlatSpec {

  "P22 given range(-10,20)" should "return a List of numbers containing the range of -10 -> 20" in {

    assert((-10 to 20).toList === P22.range(-10,20))

  }


  "P22 given range(1,10)" should "return a List of numbers containing the range of 1 to 10" in {

    assert((1 to 10).toList === P22.range(1,10))

  }

  "P22 given range(11,10)" should "throw an exception " in {

    val thrown = intercept[IllegalArgumentException](

      P22.range(11,10)

    )
   assert("Start must be less than or equal to the limit" === thrown.getMessage)

  }

  "P22 given range(10,10)" should "give back a List(10) " in {

    assert(List(10) === P22.range(10,10))
    assert((10 to 10).toList === P22.range(10,10))

  }

}

class P23Test extends FlatSpec {


  "P23 given 1 to extract from List(1)" should "return a List(1)" in {

    assert(1 === P23.randomSelect(1, List(1)).length)

  }

  "P23 given 3 to extract from List(3,4,6,8,4)" should "return a List of 3" in {

    assert(3 === P23.randomSelect(3, List(3,4,6,8,4)).length)

  }

  "P23 given 0 to extract from List(3,4,6,8,4)" should "return List()" in {
    val result = P23.randomSelect(0, List(3,4,6,8,4))
    assert(0 === result.length)
    assert(List() == result)
  }

  "P23 given -3 to extract from List(3,4,6,8,4)" should "throw an Exception" in {

    val thrown = intercept[IllegalArgumentException](P23.randomSelect(-3, List(3,4,6,8,4)))
    assert("Can't extract a negative amount of numbers" === thrown.getMessage)
  }

  "P23 given 3 to extract from List(1,2)" should "throw an Exception" in {

    val thrown = intercept[IllegalArgumentException](P23.randomSelect(3, List(3,4)))
    assert("Can't extract a larger number of items than items in the inital list" === thrown.getMessage)
  }

}

class P24Test extends FlatSpec {


  "P24 given 10 random numbers to generate from 1..10" should "return a List of 10 unique Ints, each between 1 and 10 inclusive" in {
    val result = P24.lotto(10,10)
    assert(10 === result.count(p => p > 0 && p < 11))
    assert(10 === result.distinct.length)
  }

  "P24 given 6 random numbers to generate from 1..10" should "return a List of 6 unique Ints, each between 1 and 10 inclusive" in {
    val result = P24.lotto(6,10)
    assert(6 === result.count(p => p > 0 && p < 11))
    assert(6 === result.distinct.length)
  }

  "P24 given 3 random numbers to generate from 1..10" should "return a List of 3 unique Ints, each between 1 and 10 inclusive" in {
    val result = P24.lotto(3,10)
    assert(3 === result.count(p => p > 0 && p < 11))
    assert(3 === result.distinct.length)
  }

  "P24 given -3 random numbers to generate from 1..10" should "throw an Exception" in {

    val thrown = intercept[IllegalArgumentException](
      P24.lotto(-3,10)
    )
    assert("Cannot generate a List of negative size" === thrown.getMessage)


  }

  "P24 given 3 random numbers to generate from 1..-10" should "throw an Exception" in {


    val thrown = intercept[IllegalArgumentException](
      P24.lotto(3,-10)
    )
    assert("Parameter M must be positive, cannot generate negative range for lotto" === thrown.getMessage)


  }

}

class P25Test extends FlatSpec{

  "P25  given List(1,2,3,4,5,6,7,8,9)" should " give a random permutation" in {

    val result = P25.randomPermute((1 to 9).toList)
    assert(9 === result.length)
    assert( 9 === result.distinct.count( i=> i > 0 && i < 10))
    assert(List(1,2,3,4,5,6,7,8,9) != result)

  }


}
