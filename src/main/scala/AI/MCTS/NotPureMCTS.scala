package AI.MCTS
import AI.PatternMatching.PatternMatching
import Games.Game
import Games.Baduk.{Baduk, Move}

import scala.util.Random
import scala.annotation.tailrec
import scala.collection.immutable.List

case class MCTSNode[Move](move: Games.Baduk.Move, turn: Int, children: List[MCTSNode[Games.Baduk.Move]], moves: List[Move], n: Int = 0, q: Float = 0) {
  val fullyExpanded: Boolean = moves.isEmpty
  private val c = math.sqrt(2).toFloat
  def eval(p: Int): Float = q / n + c * math.sqrt(math.log(p).toFloat / n).toFloat
}

class PureMCTS(patternMatching: PatternMatching, maxWidth: Int = 10) {
  @tailrec
  private def simulate(state: Baduk): (Int, Int) = {
    if (state.end())
      state.winner()
    else {
      val moves = state.get_moves()
      val next_state = state.go(moves(Random.nextInt(moves.length)))
      simulate(next_state)
    }
  }

  private def evaluate(count: Int): Float = count.toFloat / 81

  private def expand(state: Baduk, node: MCTSNode[Move]): (MCTSNode[Move], Int, Int, List[Move]) = {
    val move: Move = node.moves.head
    val next_state = state.go(move)
    val (winner, weigh) = simulate(next_state)
    val q = evaluate(weigh) * (if (winner == state.get_turn) 1 else if (winner == 0) 0 else -1)

    // 패턴 내림차순
    val map = patternMatching.evaluate(next_state)
    val numberToMove = (n: Int) => if (n == -1) Move.PASS else Move(n % state.get_board.size, n / state.get_board.size)
    val pair = next_state.get_moves().map(m => {
      val value = if (m == Move.PASS) 0 else map(m.toNumber(state.get_board.size))
      (value, m)
    })
    val nextStateMoves = pair.sortBy(tup => tup._1).reverse.map(tuple => tuple._2)
      .dropRight(Math.max(0, Math.min(pair.length - maxWidth, maxWidth)))

    (MCTSNode(move, state.get_turn, List(), nextStateMoves, n = 1,
      q = q), winner, weigh, node.moves.tail)
  }

  def select(state: Baduk, node: MCTSNode[Move]): (MCTSNode[Move], Int, Int) = {
    if (state.end()) {
      val tup = state.winner()
      val winner = tup._1
      val weigh = tup._2
      val q = node.q + evaluate(weigh) * (if (winner == node.turn) 1 else if (winner == 0) 0 else -1)
      (MCTSNode(node.move, state.get_turn * -1, node.children, node.moves, node.n + 1,
        q), winner, weigh)
    }
    else if !node.fullyExpanded then {
      val (child, winner, weigh, moves) = expand(state, node)
      val q = node.q + evaluate(weigh) * (if (winner == node.turn) 1 else if (winner == 0)  0 else -1 )
      val updated = node.copy(turn = -state.get_turn, children =  child :: node.children, moves =  moves, n = node.n + 1,
        q = q)
      (updated, winner, weigh)
    }
    else {
      val (max_child, max_index) = node.children.zipWithIndex.maxBy(_._1.eval(node.n))
      val (child, winner, weigh) = select(state.go(max_child.move), max_child)
      val q = node.q + evaluate(weigh) * ( if (winner == node.turn) 1 else if (winner == 0) 0 else  -1)
      val updated = node.copy(turn = -state.get_turn, children = node.children.updated(max_index, child), n = node.n + 1,
        q = q)
      (updated, winner, weigh)
    }
  }

  @tailrec
  private def searchNTimes(root_state: Baduk, node: MCTSNode[Move], n: Int): MCTSNode[Move] = {
    if (n == 0) node
    else searchNTimes(root_state, select(root_state, node)._1, n - 1)
  }


  def MCTS(game: Baduk, visits: Int): (MCTSNode[Move], Move) = {
    val map = patternMatching.evaluate(game)
    val numberToMove = (n: Int) => Move(n % game.get_board.size, n / game.get_board.size)
    val pair = map.zipWithIndex
      .filter(tup => game.get_moves().contains(Move(tup._2 % game.get_board.size, tup._2 / game.get_board.size)))
      .map(tuple => (tuple._1, numberToMove(tuple._2))).sortBy(tup => tup._1)
    val nextStateMoves = pair.reverse.map(tuple => tuple._2).toList

    val root = MCTSNode(game.null_move(), game.get_turn * -1, List(), nextStateMoves)
    val searchedRoot = searchNTimes(game, root, visits)
    //for(c <- searchedRoot.children) println(s"Move ${c.move}: ${c.q} / ${c.n}")
    val best_child = searchedRoot.children.maxBy(_.n)
    println(s"Move ${best_child.move}: ${best_child.q} / ${best_child.n} (${(best_child.q / best_child.n * 100).round}%)")
    (searchedRoot, best_child.move)
  }
}
