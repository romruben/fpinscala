package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

import ParallelParenthesesBalancing._

@RunWith(classOf[JUnitRunner])
class ParallelParenthesesBalancingSuite extends FunSuite {

  test("balance should work for empty string") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("", true)
  }

  test("balance should work for string of length 1") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("(", false)
    check(")", false)
    check(".", true)
  }

  test("balance should work for string of length 2") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("()", true)
    check(")(", false)
    check("((", false)
    check("))", false)
    check(".)", false)
    check(".(", false)
    check("(.", false)
    check(").", false)
  }

  test("[Parallel] balance should work for string") {
    val threshold = 10000
    def check(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, threshold) == expected, s"balance($input) should be $expected")

    check("(1+s5)", true)
    check("(1+1*(1kdfsk))(", false)
    check("(1+1*(1kdfsk)())", true)
    check("(check((1+1*(1kdfsk)(, true)", false)
  }

  test("[Parallel] balance should work for string with 7 parallels calls") {
    val threshold = 1

    def check(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, threshold) == expected, s"balance($input) should be $expected")

    check("()()()()", true)

  }
    test("balance should work for string") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected, s"balance($input) should be $expected")

    check("(1+s5)", true)
    check("(1+1*(1kdfsk))(", false)
    check("(1+1*(1kdfsk)())", true)
    check("(check((1+1*(1kdfsk)(, true)", false)
  }

  test("[Parallel] balance should work for string of length 2 and threshold1") {
    val threshold = 1

    def check(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, threshold) == expected, s"balance($input) should be $expected")

    check(")(", false)

  }

  test("[Parallel] balance should work for string of length 4 and threshold1") {
    val threshold = 1

    def check(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, threshold) == expected, s"balance($input) should be $expected")

    check("())())(", false)

  }

}