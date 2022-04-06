package u06lab.code

import java.util.OptionalInt

object ConnectThree extends App:
  val bound = 3
  enum Player:
    case X, O
    def other: Player = this match
      case X => O
      case _ => X

  case class Disk(x: Int, y: Int, player: Player)
  /**
   * Board:
   * y
   *
   * 3
   * 2
   * 1
   * 0
   *   0 1 2 3 <-- x
   */
  type Board = Seq[Disk]
  type Game = Seq[Board]

  import Player.*

  def find(board: Board, x: Int, y: Int): Option[Player] = board.find(p => p.x == x && p.y == y) match
    case Some(disk) => Some(disk.player)
    case _ => None

  def firstAvailableRow(board: Board, x: Int): Option[Int] = board.count(_.x == x) match
    case n if n <= bound => Some(n)
    case _ => None

  def placeAnyDisk(board: Board, player: Player): Seq[Board] =
    for
      x <- bound to 0 by -1
      y <- firstAvailableRow(board, x)
    yield board :+ Disk(x, y, player)

  def computeAnyGame(player: Player, moves: Int): LazyList[Game] = moves match
    case 0 => LazyList(Seq(Seq.empty))
    case _ =>
      val games = computeAnyGame(player.other, moves - 1)
      (for
        game <- games
        if checkWinCondition(game.head)
      yield game) ++
        (for
          game <- games
          if !checkWinCondition(game.head)
          board <- placeAnyDisk(game.head, player)
        yield board +: game)

  private[code] def checkWinCondition(board:Board): Boolean =
    checkRows(board) || checkColumns(board) || checkDiagonals(board)

  private[code] def checkRows(board: Board): Boolean =
    (for
      x <- 1 until bound
      y <- 0 to bound
    yield
      checkLine(x - 1, y, x, y, x + 1, y, board)).exists(identity)

  private[code] def checkColumns(board: Board): Boolean =
    (for
      x <- 0 to bound
      y <- 1 until bound
    yield
      checkLine(x, y - 1, x, y, x, y + 1, board)).exists(identity)

  private[code] def checkDiagonals(board: Board): Boolean =
    (for
      x <- 1 until bound
      y <- 1 until bound
    yield
      checkLine(x - 1, y - 1, x, y, x + 1, y + 1, board) || checkLine(x - 1, y + 1, x, y, x + 1, y - 1, board))
      .exists(identity)

  private[code] def checkLine(x1: Int, y1: Int, x2: Int, y2: Int, x3: Int, y3: Int, board: Board): Boolean =
    (find(board, x1, y1), find(board, x2, y2), find(board, x3, y3)) match
      case (Some(player1), Some(player2), Some(player3)) if player1 == player2 && player2 == player3 => true
      case _ => false

  def printBoards(game: Seq[Board]): Unit =
    for
      y <- bound to 0 by -1
      board <- game.reverse
      x <- 0 to bound
    do
      print(find(board, x, y).map(_.toString).getOrElse("."))
      if x == bound then
        print(" ")
        if board == game.head then println()

  // Exercise 1: implement find such that..
  println("EX 1: ")
  println(find(List(Disk(0, 0, X)), 0, 0)) // Some(X)
  println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 0, 1)) // Some(O)
  println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 1, 1)) // None

  // Exercise 2: implement firstAvailableRow such that..
  println("EX 2: ")
  println(firstAvailableRow(List(), 0)) // Some(0)
  println(firstAvailableRow(List(Disk(0, 0, X)), 0)) // Some(1)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X)), 0)) // Some(2)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X)), 0)) // Some(3)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X), Disk(0, 3, X)), 0)) // None
  // Exercise 2: implement placeAnyDisk such that..
  printBoards(placeAnyDisk(List(), X))
  // .... .... .... ....
  // .... .... .... ....
  // .... .... .... ....
  // ...X ..X. .X.. X...
  printBoards(placeAnyDisk(List(Disk(0, 0, O)), X))
  // .... .... .... ....
  // .... .... .... ....
  // ...X .... .... ....
  // ...O ..XO .X.O X..O
  println("EX 3: ")
// Exercise 3 (ADVANCED!): implement computeAnyGame such that..
  computeAnyGame(O, 4).foreach { g =>
    printBoards(g)
    println()
  }
//  .... .... .... .... ...O
//  .... .... .... ...X ...X
//  .... .... ...O ...O ...O
//  .... ...X ...X ...X ...X
//
//
// .... .... .... .... O...
// .... .... .... X... X...
// .... .... O... O... O...
// .... X... X... X... X...

// Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
