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
  def pascal(c: Int, r: Int): Int = if (c == 0 || r == c) 1 else pascal(c-1, r - 1) + pascal(c, r-1)

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def check(list: List[Char], open: Int): Boolean = {
      if (list.isEmpty) open == 0
      else if (open < 0) false
      else {
        val sum = if (list.head == ')') - 1 else + 1
        check(list.tail, open + sum)
      }
    }

    val filteredList = chars.filter(s => s == ')' || s == '(')
    check(filteredList, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def sumChange(sum: Int, tmpList: List[Int]): Int = {
      if (sum > money || tmpList.isEmpty) 0
      else if(sum == money) 1
      else sumChange(sum, tmpList.tail) + sumChange(sum + tmpList.head, tmpList)
    }

    sumChange(0, coins)
  }
}
