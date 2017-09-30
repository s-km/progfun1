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
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || r == c) 1
    else pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def parenCounter(cs: List[Char], ps: Int): Boolean =
      if (cs.isEmpty) ps == 0
      else if (ps >= 0 && cs.head == '(') parenCounter(cs.tail, ps + 1)
      else if (cs.head == ')') parenCounter(cs.tail, ps - 1)
      else parenCounter(cs.tail, ps)

    parenCounter(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0) 1
    else if (coins.isEmpty) 0
    else if (coins.head <= money)
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else countChange(money, coins.tail)

}
