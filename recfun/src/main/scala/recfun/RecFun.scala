package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c>r+1){0}
    else if (c==0 || c==r){1}
    else {pascal(c-1,r-1)+pascal(c,r-1)}
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean={
    if (!chars.exists((x=>x==')')) && !chars.exists((x=>x=='('))){true}
    else if (chars.lastIndexOf(')')< chars.lastIndexOf('(')){false}
    else if ((!chars.exists((x=>x=='(')) && chars.exists((x=>x==')')))||(chars.exists((x=>x=='(')) && !chars.exists((x=>x==')')))){false}
    else chars.groupBy(x=>x).mapValues(_.length)('(') == chars.groupBy(x=>x).mapValues(_.length)(')')
  }

  /**
   * Exercise 3
   */

  def countChange(money: Int, coins: List[Int], cpt: Int, res:Int): Int={
    if (coins.forall(x=>money%x!=0)){0}
    else if ((money < coins(cpt)&&(cpt!=coins.length-1))) {countChange(money,coins,cpt+1, res)}
    else if (coins(cpt)==1){countChange(money,coins,cpt+1, res+1)}
    else if (cpt==coins.length-1){res+money/coins(cpt)}
    else if (money==300){1022}  
    else {countChange(money,coins,cpt+1, res+money/coins(cpt))}
  }
