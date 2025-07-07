/*
Sets vs Seqs
1. Sets are unordered: elems do not have predefined order of appearance
2. Do not have duplicate elems
3. Fundamental set op is contains
 */

/*
N-Queens
Place n queens on a chessboard of n size s.t. no queen threatenend by another
i.e. no 2 queens in same row, col or diag
 */

def queens(n: Int) =
  /** @k:
    *   current queen to place
    * @return:
    *   set of all solutions consisting of placing k queens on a board size n
    *   Generate all possible extensions of each sol preceded by a new queen pos
    */
  def placeQueens(k: Int): Set[List[Int]] =
    if k == 0 then Set(List())
    else
      for
        queens <- placeQueens(k - 1)
        col <- 0 until n
        if isSafe(col, queens)
      yield col :: queens

  /** check if queen can be placed in col w/o being in same row col or diagonal
    * as another queen already placed
    * @param col:
    *   current column considering placing queen
    * @param queensPlaced:
    *   cols of queens already placed in desc order of placement
    * @return
    *   whether queen can be placed w/o threatening other queens
    */
  def isSafe(col: Int, queensPlaced: List[Int]): Boolean =
    // if in diagonal
    def inDiagonal(col: Int, existingQueenCol: Int, rowsAbove: Int) =
      (existingQueenCol - col).abs == rowsAbove

    // if in same col or in diagonal
    !(queensPlaced.contains(col) || queensPlaced.zipWithIndex.exists((q, i) =>
      inDiagonal(col, q, i + 1)
    ))

  placeQueens(n)

def isSafeMartin(col: Int, queens: List[Int]): Boolean =
  def checks(col: Int, delta: Int, queens: List[Int]): Boolean =
    queens match
      case qcol :: others =>
        qcol == col // vertical check
        || (qcol - col).abs == delta // diagonal check
        || checks(col, delta + 1, others)
      case Nil => false
  !checks(col, 1, queens)

queens(4)
