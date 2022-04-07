package u06lab.code

import java.util.OptionalInt
import u06lab.code.GridGameLogic

object ConnectThree extends App with GridGameLogic:

  override val bound = 3

  override def placeAnyDisk(board: Board, player: Player): Seq[Board] =
    for
      x <- 0 to bound
      y <- firstAvailableRow(board, x)
    yield board :+ Disk(x, y, player)

  @main
  def main(): Unit =
    import Player._

    computeAnyGame(O, 4).foreach { g =>
      printBoards(g)
      println()
    }
