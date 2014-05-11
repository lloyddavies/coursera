package recfun
import common._

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
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def bracketCount(chars: List[Char], c: Int): Int = {
      if (c < 0 || chars.isEmpty) c
      else if (chars.head == '(') bracketCount(chars.tail, c + 1)
      else if (chars.head == ')') bracketCount(chars.tail, c - 1)
      else bracketCount(chars.tail, c)
    }
    bracketCount(chars, 0) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def collectChangeCombinations(money: Int, coins: List[Int], sum: Int): Int = {
      if (sum == money) 1
      else if (coins.isEmpty || sum > money) 0
      else collectChangeCombinations(money, coins, sum + coins.head) + collectChangeCombinations(money, coins.tail, sum)
    }
    collectChangeCombinations(money, coins, 0)
  }
}