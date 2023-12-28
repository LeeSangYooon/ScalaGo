package Games.Baduk

import AI.MCTS.PureMCTS
import Games.Baduk.Baduk.{BLACK, EMPTY, WHITE, newGame}
import Games.Game
import AI.Heuristic.MergeMaps
import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Stack}

case class Move(x: Int, y: Int) {
  lazy val is_pass: Boolean = this == Move.PASS
  def toNumber(size: Int): Int = x + y * size
  override def toString: String =
    if (is_pass) "PASS"
    else ('A'.toInt + x).toChar + (y + 1).toString
}

object Move {
  val PASS: Move = Move(-1, -1)
}

case class Group(stones: Array[Move], lives: Int, connectedGroups: mutable.HashMap[Move, Float] = mutable.HashMap()) {
  def merge(other: Group): Group = {
    val map = MergeMaps.mergeMaps(connectedGroups, other.connectedGroups)
    val sumMap = map.filterNot((k, v) => stones.contains(k) || other.stones.contains(k))
    Group(stones ++ other.stones, lives + other.lives, sumMap)
  }

  override def toString: String = /*f"Group(${stones.mkString("Array(", ", ", ")")}, lives: $lives, connections: ${connectedGroups.map(g=>(g._1, g._2))})"*/
    stones.mkString("Group(", " ", ")")
}

class Board(size_init: Int = 9, board: Array[Int] = Array.fill(81)(0)) {
  val size: Int = size_init
  def get(move: Move): Int = board(index(move))
  def updated(move: Move, color: Int): Board = Board(size_init, board.updated(index(move), color))
  def neighbors(move: Move): List[Move] = {
    (for (n <- Seq(Move(move.x - 1, move.y), Move(move.x, move.y - 1), Move(move.x + 1, move.y), Move(move.x, move.y+ 1))
         if in(n))
      yield n).toList
  }
  def switched(): Board = {
    Board(size, board.map(a => if (a==WHITE) BLACK else if (a == BLACK) WHITE else EMPTY))
  }
  def check_and_remove(move: Move): (Board, Int) = {
    val color = get(move)
    if (neighbors(move).exists(m => get(m) == Baduk.EMPTY)) (this, 0)
    else {
      val visited = Array.fill(size_init * size_init)(false)
      val dead_stones = mutable.Stack[Move]()
      val stack = mutable.Stack[Move](move)
      while (stack.nonEmpty) {
        val top = stack.pop()
        dead_stones.push(top)
        visited(index(top)) = true
        if (neighbors(top).exists(m => get(m) == Baduk.EMPTY))
          return (this, 0)
        for (n <- neighbors(top); if color == get(n) && !visited(index(n)))
          stack.push(n)
      }
      val new_array = Array(board: _*)
      for (stone <- dead_stones) {
        new_array(stone.x + stone.y * size) = 0
      }
      (Board(size, new_array), dead_stones.length)
    }


  }

  def collectGroups: ListBuffer[Group] = {
    val visited = Array.fill(size_init * size_init)(false)
    val groups: ListBuffer[Group] = ListBuffer()
    for (x <- 0 until size; y <- 0 until size; m = Move(x, y);
         if get(m) != Baduk.EMPTY;
         color = get(m)) {
      if (!visited(x + y * size)) {
        visited(x + y * size) = true
        var count: Int = neighbors(m).count(move => get(move) == Baduk.EMPTY)
        var group: Array[Move] = Array(m)
        val stack: mutable.Stack[Move] = mutable.Stack()
        for (a <- neighbors(m))
          stack.push(a)
        while (stack.nonEmpty) {
          val top = stack.pop()
          if (get(top) == color && !visited(top.toNumber(size))) {
            visited(top.toNumber(size)) = true
            count += neighbors(top).count(move => get(move) == Baduk.EMPTY)
            group :+= top
            for (n <- neighbors(top))
              stack.push(n)
          }
        }
        groups.append(Group(group, count))
      }

    }
    groups
    // 완성 후 Eval 구현
  }
  private def in(move: Move): Boolean = 0 <= move.x && move.x < size_init && 0 <= move.y && move.y < size_init
  def at(n: Int): Int = board(n)
  def index(move: Move): Int = move.y * size_init + move.x
}



class Baduk(turn: Int = 1, board: Board = Board(),
            b_cap: Int = 0, w_cap: Int = 0, ko: Option[Move] = None,
            passed: Boolean = false, is_end: Boolean = false, komi: Float = 6.5f,
            moves: Int = 0) extends Game[Move] {
  private val limit: Int = board.size * board.size * 3

  val blackCaptures = b_cap
  val whiteCaptures = w_cap
  val movesCount = moves

  def get_board: Board = board

  def get_turn: Int = turn

  def get_komi: Float = komi

  def get_moves(): List[Move] = {
    Move.PASS :: (ko match
      case Some(k) => {
        for (x <- 0 until board.size; y <- 0 until board.size;
             move = Move(x, y); if board.get(move) == 0 && move != k) yield move
      }

      case _ =>
        for (x <- 0 until board.size; y <- 0 until board.size; move = Move(x,y); if board.get(move) == 0) yield move
      ).toList
  }

  def go(move: Move): Baduk = {
    if (move.is_pass)
      return Baduk(switch_turn(turn), board, b_cap, w_cap, None, true, passed || moves > limit, komi, moves + 1)

    val neighbors = board.neighbors(move)
    var removed_board: Board = board.updated(move, turn)
    var b_cap_delta = 0
    var w_cap_delta = 0
    var one_of_removed: Move = null

    // remove opponent's stones first
    for (n <- neighbors if board.get(n) == switch_turn(turn)) {
      val (rb, removed_stones) = removed_board.check_and_remove(n)
      removed_board = rb
      if (removed_stones != 0) {
        one_of_removed = n
      }
      turn match
        case BLACK => b_cap_delta += removed_stones
        case WHITE => w_cap_delta += removed_stones
    }

    // check and removes itself
    val (rb, self_remove_stones) = removed_board.check_and_remove(move)
    removed_board = rb
    turn match
      case BLACK => w_cap_delta += self_remove_stones
      case WHITE => b_cap_delta += self_remove_stones

    val is_ko = self_remove_stones == 0 &&
      b_cap_delta + w_cap_delta == 1 &&
      removed_board.neighbors(move).count(m => removed_board.get(m) == EMPTY) == 1 &&
      !removed_board.neighbors(move).exists(m => removed_board.get(m) == turn)

    Baduk(switch_turn(turn), removed_board, b_cap + b_cap_delta, w_cap + w_cap_delta,
      if (is_ko) Some(one_of_removed) else None, false, moves > limit, komi, moves + 1)
  }

  def end(): Boolean = is_end

  def winner(): (Int, Int) = {
    val visited = Array.fill(board.size * board.size)(false)
    var b_count = b_cap
    var w_count = w_cap + komi

    def search_liberties(move: Move, color: Int): Option[Int] = {

      if (visited(board.index(move))) return Some(0)
      var count = 1
      visited(board.index(move)) = true
      var no = false
      for (n <- board.neighbors(move) if !no && !visited(board.index(n))) {
        if (board.get(n) == EMPTY) {
          search_liberties(n, color) match
            case Some(i) => {
              count += i
            }
            case _ => {}
        } else if (board.get(n) == color){
          visited(board.index(n)) = true
        } else {
          no = true
        }
      }
      if (no) return None
      Some(count)
    }

    for {i <- 0 until board.size
         j <- 0 until board.size
         p = Move(i, j)
         color = board.get(p)
         if color != EMPTY}
    {
      var delta = 0
      for (n <- board.neighbors(p) if board.get(n) == EMPTY)
        search_liberties(n, color) match
          case Some(i) => delta += i
          case _ => {}
      color match
        case BLACK => b_count += delta
        case WHITE => w_count += delta
    }
    if (b_count > w_count)
      (BLACK, ((b_count - w_count) * 2).toInt)
    else
      (WHITE, ((w_count - b_count) * 2).toInt)
  }

  def null_move(): Move = Move(-2, -2)

  override def to_vector(for_whose_turn: Int): Array[Double] = {
    val array: Array[Double] = Array.fill(board.size * board.size + 1)(0)
    for (i <- 0 until board.size * board.size){
      array(i) = board.at(i) * for_whose_turn
    }
    array(board.size * board.size) = if (for_whose_turn == BLACK) b_cap - w_cap else w_cap - b_cap
    array
  }

  override def get_best_moves(values: Array[Double], number: Int): List[Move] = {
    var moves = List[(Move, Double)]()
    for (i <- 0 until board.size; j <- 0 until board.size; move = Move(j, i)) {
      val notKo = ko match
        case Some(k) => k != move
        case _  => true
      if (board.get(move) == EMPTY && notKo)
        moves = (move, values(board.index(move))) :: moves
    }
    moves = (Move.PASS, values(board.size * board.size)) :: moves
    val del = if moves.length > number then moves.length - number else 0
    moves.sortWith((a,b) => a._2 > b._2).map(a => a._1).dropRight(del)
  }

  override def turn_switched(): Baduk = {
    Baduk(switch_turn(turn), board.switched(), w_cap, b_cap, ko, passed, false, komi, moves)
  }

  def show(): Unit = {
    for (i <- 0 until board.size) {
      for (j <- 0 until board.size){
        val color = board.get(Move(j, i))
        print(if (color == EMPTY) 'E' else if (color == BLACK) 'B' else 'W')
      }
      println()
    }
    println(s"BLACK CAPTURES: $b_cap, WHITE CAPTURES: $w_cap")
  }

  private def switch_turn(t: Int): Int = t * -1
}

object Baduk {
  def newGame(size:Int = 9): Baduk = Baduk(1, Board(size, Array.fill(size * size)(0)))
  val EMPTY: Int = 0
  val BLACK: Int = 1
  val WHITE: Int = -1
}
