package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] =  for {
    a <- arbitrary[Int]
    h <- frequency((1, Gen.const(empty)), (9, genHeap))//oneOf(Gen.const(empty), genHeap)
  } yield insert(a, h)
    //A == Int and H  == List[Node]


  given Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  property("min one element") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min ab") = forAll { (a: Int , b:Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b,h1);
    findMin( h2 ) == a.min(b)
  }

  property("isempty") = forAll { (a: Int ) =>
    val h1 = insert(a, empty)
    val h2 = deleteMin(h1)
    isEmpty(h2)
  }

  property("ordered") = forAll { (h: H ) =>
    def orderedList(h: H):List[A]= {
    h match{
      case Nil => Nil
      case _ => {
        val m=findMin(h)
        val o=deleteMin(h)
        m::orderedList(o)
      }
    }

    }
    def isordered(l:List[A]):Boolean={
      if (l.length==1) true
      else if (l.head <= l.tail.head) isordered(l.tail)
      else false
  }
    isordered(orderedList(h))
  }


  property("mins ") = forAll { (h1: H, h2:H ) =>
    val h3 = meld(h1,h2)
    val m1 = findMin(h1)
    val m2= findMin(h2)
    val m3=findMin(h3)
    m3==m1.min(m2)
  }

  property("The minimal value of a melded Heap should be the min of the min of both heaps") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == findMin(h1).min(findMin(h2))
  }


  property("Two heaps should be equal if recursivly removing min elements result in same elements until empty") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && heapEqual(deleteMin(h1), deleteMin(h2))
      }
    heapEqual(meld(h1, h2),
      meld(deleteMin(h1), insert(findMin(h1), h2)))
  }

  property("The minimal value of 2 heaps should be the minimal after dispacing it from heap 1 to 2 and melding both") = forAll { (h1: H, h2: H) =>
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    val m = m1.min(m2)
    findMin(meld(deleteMin(h1), insert(m, h2))) == m
  }
