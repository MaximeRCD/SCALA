

// Representation of JSON
/*abstract  class JSON
object JSON {
  case class Seq(elems: List[JSON]) extends JSON

  case class Obj(bindings: Map[String, JSON]) extends JSON

  case class Num(num: Double) extends JSON

  case class Str(str: String) extends JSON

  case class Bool(b: Boolean) extends JSON

  case class Null() extends JSON
}

val jsdata=JSON.Obj(Map(
  "firsname"->JSON.Str("Maxime"),
  "Lastname" -> JSON.Str("RICHAUDEAU"),
  "Age"-> JSON.Num(21),
  "Adresses mails" -> JSON.Seq(List(
    JSON.Obj(Map(
      "Perso"->JSON.Str("maxime.r1@gmail.fr"),
      "Professionel" -> JSON.Str("m.r@axa-direct.com")
    )))),
  "Man"-> JSON.Bool(true),
  "Women" -> JSON.Bool(false)
))
def inquotes(str:String):String={"\""+str+"\""}

def show(js:JSON):String= js match {
  case JSON.Seq(elems)=>elems.map(show).mkString("[",",","]")
  case JSON.Obj(bindings)=>
    val associations = bindings.map{
      case (key,value)=>s"${inquotes(key)} : ${show(value)} "
    }
    associations.mkString("{",", \n","}")

  case JSON.Num(num)=>num.toString
  case JSON.Str(str)=>inquotes(str)
  case JSON.Bool(b)=>b.toString
  case JSON.Null() =>"null"
}

show(jsdata)*/

def abs( x:Int):Int={
  if (x<0) -x else x
}

// random values

trait Generator[T]
{
  def generate(): T
  def map[S](f:T=>S)=new Generator[S] {
    def generate()=f(Generator.this.generate())
  }
  def flatMap[S](f:T=>Generator[S])=new Generator[S] {
    def  generate()=f(Generator.this.generate()).generate() }
}


import java.util._

val integers: Generator[Int] = new Generator[Int] {
  val rand = new Random()
  def generate()= rand.nextInt()
}

val booleans=new Generator[Boolean] {
  def generate()=integers.generate() >0
}

val pairs = new Generator[(Int, Int)] {
  def generate()=(integers.generate(),integers.generate())
}

booleans.generate()
pairs.generate()



def single[T](x:T) :Generator[T] =  {
  () => x
}


def range (lo:Int,hi:Int) : Generator[Int]={ for {x <- integers} yield lo + abs(x) % (hi - lo)}

def oneOf[T](xs:T*):Generator[T]= for (idx <- range(0,xs.length))  yield xs(idx)

val choice=oneOf("red","blue","green")

choice.generate()

// List generator

lazy val lists: Generator[scala.List[Int]] = {
  def emptyLists = single(scala.List[Int]())
  def nonEmptyLists = for(head<- integers; tail<-lists) yield head :: tail

  for
  (isEmpty <- oneOf[Int](0,10);
   list <- if (isEmpty==0) emptyLists else nonEmptyLists )
  yield list
}

lists.generate()



abstract class Tree
object Tree {
  case class inner(left:Tree,right:Tree) extends Tree
  case class leaf(x:Int ) extends Tree
}

def trees:Generator[Tree]={
  def inners = for (x <- trees;y<- trees) yield Tree.inner(x,y)
  def leafs =for (x<- integers) yield Tree.leaf(x)

  for
  (isLeaf <- booleans ;
   tree <- if (isLeaf) {leafs} else {inners})
  yield tree
}

trees.generate()


