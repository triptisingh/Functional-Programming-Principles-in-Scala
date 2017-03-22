package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    val ex: List[Char] = List('c', 'd', '(', '(', ')', ')')
    println (balance(ex))
    val currenyList: List[Int] = List(1, 2)
    println (countChange(4, currenyList))
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c <= 0 || c == r || r <= 0) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  }

  /**
    * Exercise 2
    */

  def balance(chars: List[Char]): Boolean = {
    def recur(chars: List[Char], count: Int): Boolean = {
      if (chars.isEmpty) count == 0
      else if (chars.head == '(') recur(chars.tail, count + 1)
      else if (chars.head == ')') recur(chars.tail, count - 1)
      else recur(chars.tail, count)

    }
    recur(chars, 0)
  }


  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money > 0 && coins.nonEmpty)
        countChange(money - coins.head, coins) + countChange(money, coins.tail)
      else 0

  }

}



