package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for row <- 0 to 10 do for col <- 0 to row do print(s"${pascal(col, row)} ")
    println()
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c <= 0 || c == r)
      return 1
    pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def countParens(chars: List[Char], countOpen: Int): Boolean = {
      chars match {
        case Nil       => countOpen == 0
        case '(' :: cs => countParens(cs, countOpen + 1)
        case ')' :: cs => {
          if (countOpen == 0)
            return false
          return countParens(cs, countOpen - 1)
        }
        case _ :: cs => countParens(cs, countOpen)
      }
    }

    countParens(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???

}
