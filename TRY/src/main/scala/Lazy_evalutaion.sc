// Priciple of induction

abstract class IntSet{
  def incl(x:Int):IntSet
  def contains(x:Int):Boolean
}
case class NonEmpty(elem:Int,left:IntSet,right:IntSet) extends IntSet {
  override def contains(x:Int):Boolean={
    if (x < elem) left.contains(x)
    else if (x > elem) right.contains(x)
    else true
  }

  override def incl(x: Int): IntSet = {
    if(x<elem) NonEmpty(elem,left.incl(x),right)
    else if (x>elem) NonEmpty(elem,left,right.incl(x))
    else this
  }
}
object Empty extends IntSet {
  override def incl(x:Int):IntSet = NonEmpty(x,Empty,Empty)
  override def contains(x: Int): Boolean = false
}


def isPrime(x:Int):Boolean={
  (2 until x).forall(x%_!=0)
}


/* The trick is that we want to avoid computing the elements of a sequence until they are needed for the evaluation
result, which might be never. This idea is implemented in a new class and this Scala Standard Library,
which is a lazy List. Lazy Lists are similar to lists but the elements are evaluated only on demand.
 */
(1000 to 10000).filter(isPrime).head
(1000 to 10000).to(LazyList).filter(isPrime)(100)

/*There is however, an alternative operator, hash cons, which
 produces a LazyList, so x #:: xs is the same thing as LazyList.cons(x, xs).
 */


/* Problem of water pouring */

type Glass = Int
type State = Vector[Int]

class Pouring(full:State)={
  trait Move {
    case Empty(glass: Glass)
    case Fill(glass: Glass)
    case Pour(from: Glass, to: Glass)

    def apply(state: State): State = this match {
      case Empty(glass) => state.updated(glass, 0)
      case Fill(glass) => state.updated(glass, full(glass))
      case Pour(g1, g2) =>
        val amount = state(g1).min(full(g2) - state(g2))
        state.updated(g1, state(g1) - amount).updated(g2, state(g2) + amount)
    }
  }
  val moves = {
  val glasses: Range = full.indices
  (for (g <- glasses) yield Moves.this.Empty(g))++(for (g <- glasses) yield Moves.this.Fill(g))++(for (g1 <- glasses; g2 <- glasses if g1 != g2) yield Moves.this.Pour(g1, g2))
  }
  class Path(history:List[Moves], endState:State){
  def extend(move:Moves):Path=Path(move::history,move(endState))
  override def toString:String=s"${history.reverse.mkString(" ")}-->$endState"
  }
  val empty:State=full.map(_=>0)
  val start=Path(Nil,empty)

  def pathsFrom(paths:List[Path], explored:Set[State]):LazyList[List[Path]]={
  val frontier = {
    for (path <- paths; move <- moves; next<-path.extend(move) if !explored.contains(next.endState)) yield next
    paths #::pathsFrom(frontier,explored++frontier.map(_.endState))
  }
  }

  def solutions(target:Int):LazyList[Path]=
  for
  ( paths<-pathsFrom(List(start),Set(empty));
    path<-paths if path.endState.contains(target))
  yield path }
}

val problem = Pouring(Vector(4,7))
problem.solutions(6)
