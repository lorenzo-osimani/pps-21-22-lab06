package u06lab.code

trait GridGameLogic:
  enum Player:
    case X, O
    def other: Player = this match
      case X => O
      case _ => X

  case class Disk(x: Int, y: Int, player: Player)

  type Board = Seq[Disk]
  type Game = Seq[Board]

  import Player.*

  def bound: Int

  def find(board: Board, x: Int, y: Int): Option[Player] = board.find(p => p.x == x && p.y == y) match
    case Some(disk) => Some(disk.player)
    case _ => None

  def firstAvailableRow(board: Board, x: Int): Option[Int] = board.count(_.x == x) match
    case n if n <= bound => Some(n)
    case _ => None

  def placeAnyDisk(board: Board, player: Player): Seq[Board]

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
