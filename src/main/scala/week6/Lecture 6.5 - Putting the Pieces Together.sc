import scala.::
/*
Assume given a dictionary words as list of words
Design a method encode
s.t. encode(phoneNumber) produces all phrases of words
that can serve as mnemonics for phone number
 */
class Coder(words: List[String]):
  val mnemonics =
    Map(
      '2' -> "ABC",
      '3' -> "DEF",
      '4' -> "GHI",
      '5' -> "JKL",
      '6' -> "MNO",
      '7' -> "PQRS",
      '8' -> "TUV",
      '9' -> "WXYZ"
    )

  /** Maps a letter to the digit it represents
    */
  private val charCode: Map[Char, Char] =
    for
      (digit, chars) <- mnemonics
      c <- chars
    yield c -> digit

  /** Maps a word to the digit string it can represent
    */
  private def wordCode(word: String): String = word.toUpperCase.map(charCode)

  /** Maps a digit string to all words in the dictionary that represent it
    */
  private val wordsForNum: Map[String, List[String]] =
    words.groupBy(wordCode).withDefaultValue(Nil)

  /** All ways to encode a number as a list of words
    */
  /* Use divide and conquer:
  reduce problem by 2 case distinction:
  1. numbers empty or not
  2. where we have split point to a simpler problem with recusive call, fewer numbers
  */

  def encode(number: String): Set[List[String]] =
    if number.isEmpty then Set(Nil)
    else
      for
        splitPoint <- (1 to number.length).toSet // point where we want to split number, toSet as want to return Set
        word <- wordsForNum(number.take(splitPoint)) // compute the word that contains the digits up to split point
        rest <- encode(number.drop(splitPoint)) // compute rest of phrase that contains digits after split point
      yield word :: rest // compose

@main def code(number: String) =
  val coder = Coder(List("Scala", "Python", "Ruby", "C", "rocks", "socks", "sucks", "works", "pack"))
  coder.encode(number).map(_.mkString(" "))

code("7225276257")
