def isordered(l:List[Int]):Boolean={
  if (l.length==1) true
  else if (l.head <= l.tail.head) isordered(l.tail)
  else false
}

def min(xs: List[Int]):Int={

  val m =xs.head
  if (xs.forall(x=>x>=m) || xs.length==1){m}
  else {min(xs.drop(1))}
}
def remove(num: Int, list: List[Int]) = list diff List(num)


def orderedList(h: List[Int]):List[Int]= {
  h match{
    case Nil => Nil
    case _ =>
      val m=min(h)
      val o=remove(m,h)
      m::orderedList(o)
  }
}

orderedList(List(-3,1,6,-24,3,4,6))
isordered(orderedList(List(-3,1,6,-24,3,4,6)))


val h3 = List.concat(List(-3,1,6,-24,3,4,6),List())
val m1 = min(List(-3,1,6,-24,3,4,6))
val m2= min(List())
val m3=min(h3)
m3==m1.min(m2)