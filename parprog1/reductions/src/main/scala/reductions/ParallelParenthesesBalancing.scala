package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }

    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def check(list: Array[Char], open: Int): Boolean = {
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

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    @tailrec
    def traverse(idx: Int, until: Int, open: Int) : Int = {
      if (idx >= until) open
      else chars(idx) match {
        case '(' => traverse(idx + 1, until, open + 1)
        case ')' => traverse(idx + 1, until, open - 1)
        case _ => traverse(idx + 1, until, open)
      }
    }

    def reduce(from: Int, until: Int) : Int = {
      if((until - from) <= threshold) traverse(from, until, 0)
      else{
        val mid = from + ((until - from) / 2)
        val (left, right) = parallel(reduce(from, mid), reduce(mid, until))
        left + right
      }
    }

    if(chars(0) == ')') false else reduce(0, chars.length) == 0
  }



  // For those who want more:
  // Prove that your reduction operator is associative!

}
