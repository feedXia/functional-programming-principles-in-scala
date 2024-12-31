package recfun
import scala.annotation.tailrec

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do print(s"${pascal(col, row)} ")
      println()

  /** Exercise 1
    */
  def pascal(c: Int, r: Int): Int =
    if c < 0 || r < 0 then
      throw new IllegalArgumentException(
        "Column & row number cannot be negative."
      )
    else if r == 0 || c == 0 || c == r then 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /** Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def parenthesisToNum(char: Char) = char match {
      case '(' => 1
      case ')' => -1
      case _   => 0
    }

    @tailrec
    def trackOpenClose(
        current: Char,
        next: List[Char],
        counter: Int
    ): Boolean = {
      if counter < 0 then false
      else {
        next match {
          case Nil => (counter + parenthesisToNum(current)) == 0
          case head :: tail =>
            trackOpenClose(head, tail, counter + parenthesisToNum(current))
        }
      }
    }

    chars match {
      case Nil                      => true
      case _ if chars.size % 2 == 1 => false
      case head :: tail             => trackOpenClose(head, tail, 0)
    }
  }

  /** Exercise 3
    */
//  def countChange(money: Int, coins: List[Int]): Int = {
//    if money == 0 then 1
//    else if money > 0 && coins.nonEmpty then
//      countChange(money - coins.head, coins) + countChange(money, coins.tail)
//    else 0
//  }

case class Candidates(coins: Map[Int, Int], remaining: Int)
def countChange(money: Int, coins: List[Int]): Int = {
  def helper(coins: List[Int], acc: List[Candidates]): Int = {
    coins match {
      case Nil => acc.count(_.remaining == 0)
      case h :: t => {
        val newAcc = acc.flatMap { sol =>
          val numCoins = sol.remaining / h
          (0 to numCoins).map(n =>
            Candidates(sol.coins ++ Map((h, n)), sol.remaining - h * n)
          )
        }
        helper(t, newAcc)
      }
    }
  }

  // List cannot be empty else flatmap will not iterate
  helper(coins, List(Candidates(Map.empty, money)))
}

  /**
   * countChange(4,List(1,2))
   * countChange(4 - 1,List(1,2)) + countChange(4,List(2))
   * countChange(3,List(1,2)) + countChange(4,List(2))
   * countChange(3 - 1,List(1,2)) + countChange(3,List(2)) + countChange(4 - 2,List(2)) + countChange(4,List())
   * countChange(2,List(1,2)) + countChange(3,List(2)) + countChange(2,List(2)) + countChange(4,List())
   * countChange(2 - 1,List(1,2)) + countChange(2,List(2)) + countChange(3 - 2,List(2)) + countChange(3,List()) + countChange(2 - 2,List(2)) + countChange(2,List()) + 0
   * countChange(1,List(1,2)) + countChange(2,List(2)) + countChange(1,List(2)) + 0 + countChange(0,List(2)) + 0
   * countChange(1 - 1,List(1,2)) + countChange(1,List(2)) + countChange(2 - 2,List(2)) + countChange(2,List()) + countChange(1 - 2,List(2)) + countChange(1,List()) + 1 
   * countChange(0,List(1,2)) + countChange(1 - 2,List(2)) + countChange(1,List()) + countChange(0,List(2)) + 0 + countChange(-1,List(2)) + 0 + 1
   * 1 + countChange(-1,List(2)) + 0 + 1 + 0 + 1
   * 1 + 0 + 1 + 1
   * 3
  */

