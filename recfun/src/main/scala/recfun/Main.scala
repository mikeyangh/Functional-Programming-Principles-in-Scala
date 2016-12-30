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
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || c== r) 1 else pascal(c-1, r-1)+pascal(c, r-1)
  }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def helper(chars: List[Char], open: Int): Boolean = {
        if (open < 0) return false
        if (chars.isEmpty) return open == 0

        if (chars.head == ')') helper(chars.tail, open-1)
        else if (chars.head == '(') helper(chars.tail, open+1)
        else helper(chars.tail, open)
      }

      helper(chars, 0)
  }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) {
        return 1
      }
      if (money < 0 || coins.isEmpty) {
        return 0
      }

      countChange(money, coins.tail)+countChange(money-coins.head, coins)
  }
  }
