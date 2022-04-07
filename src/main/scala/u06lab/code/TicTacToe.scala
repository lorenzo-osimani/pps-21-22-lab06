package u06lab.code

object TicTacToe extends App with GridGameLogic:

  override def bound = 2

  override def placeAnyDisk(board: Board, player: Player): Seq[Board] =
    for
      x <- bound to 0 by -1
      y <- bound to 0 by -1
      if find(board, x, y).isEmpty
    yield board :+ Disk(x, y, player)
  

  computeAnyGame(Player.O, 3).foreach { g =>
    printBoards(g)
    println()
  }