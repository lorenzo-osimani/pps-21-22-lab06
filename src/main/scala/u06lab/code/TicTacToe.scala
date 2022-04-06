package u06lab.code

import ConnectThree.*

extension (c: ConnectThree)
  override def placeAnyDisk(board: Board, player: Player): Seq[Board] =
    for
      x <- bound to 0 by -1
      y <- bound to 0 by -1
      if find(board, x, y).isEmpty
    yield board :+ Disk(x, y, player)

