package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = if(c==0 || c==r) 1 else pascal(c-1,r-1) + pascal(c,r-1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def stack(chars:List[Char],braces:List[Char]): List[Char] ={
        if(chars.length == 0)
          braces

        else if(chars(0)=='(')
          stack(chars.drop(1),'(' :: braces)

        else if(chars(0)==')')

          if(braces.isEmpty)
            ')'::List.empty

          else
            stack(chars.drop(1),braces.drop(1))

        else
          stack(chars.drop(1),braces)
      }
      stack(chars,List.empty).isEmpty
    }
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = if(money==0) 1 else if(money<0 || coins.length==0) 0 else countChange(money-coins(0),coins) + countChange(money,coins.drop(1))
  }
