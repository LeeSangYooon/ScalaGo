package AI.MCTS
import AI.Heuristic.{Eval, Policy}
import Games.Baduk.{Baduk, Move}

import scala.util.Random
import scala.annotation.tailrec
import scala.collection.immutable.List

object MCTS {
  private val c = math.sqrt(2).toFloat
  case class MCTSNode(move: Move, turn: Int, children: List[MCTSNode], moves: List[Move], n: Int = 0, q: Float = 0) {
    val fullyExpanded: Boolean = moves.isEmpty

    def eval(p: Int): Float = q / n + c * math.sqrt(math.log(p).toFloat / n).toFloat

    override def toString: String = f"$move: $q / $n"
  }

  private def getBestNMoves(game: Baduk, n: Int, color: Int): List[Move] = {
    val size = game.get_board.size
    val moves =
      if (game.movesCount > 5) game.get_moves()
      else if (game.movesCount > 3) game.get_moves().filterNot(m=> m.x == 0 || m.x == size - 1 || m.y == 0 || m.y == size - 1)
      else game.get_moves().filterNot(m=> m.x <= 1 || m.x >= size - 2 || m.y <= 1 || m.y >= size - 2)

    val scoresPair = moves.map(m => (Eval(game.go(m)).valueBy(color), m))
    scoresPair.sortBy(tup => -tup._1).slice(0, Math.min(n, moves.length-1)).map(tup => tup._2)
  }

  private def filterMoves(moves: List[Move], game: Baduk): List[Move] = {
    val size = game.get_board.size
    val moves =
      if (game.movesCount > 5) game.get_moves()
      else if (game.movesCount > 3) game.get_moves().filterNot(m => m.x == 0 || m.x == size - 1 || m.y == 0 || m.y == size - 1)
      else game.get_moves().filterNot(m => m.x <= 1 || m.x >= size - 2 || m.y <= 1 || m.y >= size - 2)

    moves
  }

  @tailrec
  private def simulate(state: Baduk, limit: Int): Int = {
    if (state.end() || limit <= 0)
      Eval(state).getWinner
    else {
      val moves = filterMoves(state.get_moves(), state)
      val next_state = state.go(moves(Random.nextInt(moves.length)))
      simulate(next_state, limit - 1)
    }
  }

  private def expand(state: Baduk, node: MCTSNode): (MCTSNode, Int, List[Move]) = {
    val move = node.moves(Random.nextInt(node.moves.length))
    val next_state = state.go(move)
    val winner = simulate(next_state, 100)
    val policyMoves = Policy(next_state).moves
    (MCTSNode(move, state.get_turn, List(), filterMoves(policyMoves.toList, next_state)/*getBestNMoves(next_state, 10, next_state.get_turn)*/, n = 1,
      q = if (winner == state.get_turn) 1 else if (winner == 0) 0.5f else 0), winner,
      node.moves.filterNot(elm => elm == move))
  }

  def select(state: Baduk, node: MCTSNode): (MCTSNode, Int) = {
    if (state.end()) {
      val winner = state.winner()._1
      (MCTSNode(node.move, state.get_turn * -1, node.children, node.moves, node.n + 1,
        if (winner == node.turn) node.q + 1 else if (winner == 0) node.q + 0.5f else node.q), winner)
    }
    else if (!node.fullyExpanded) {
      val (child, winner, moves) = expand(state, node)
      (MCTSNode(node.move, state.get_turn * -1, child :: node.children, moves, node.n + 1,
        if (winner == node.turn) node.q + 1 else if (winner == 0) 0.5f else node.q), winner)
    }
    else {
      val (max_child, max_index) = node.children.zipWithIndex.maxBy(_._1.eval(node.n))
      val (child, winner) = select(state.go(max_child.move), max_child)
      (MCTSNode(node.move, state.get_turn * -1, node.children.updated(max_index, child), node.moves, node.n + 1,
        if (winner == node.turn) node.q + 1 else if (winner == 0) node.q + 0.5f else node.q), winner)
    }
  }

  @tailrec
  private def searchNTimes(root_state: Baduk, node: MCTSNode, n: Int, visitSkip: Boolean=true, totalVisits: Int): MCTSNode = {
    if (n % 1000 == 0)
      print('.')
    if (n == 0) node
    else
    {
      if (visitSkip && node.children.length >= 2 && n % 100 == 0) {
        val max = node.children.maxBy(c => c.n)
        val second = node.children.filterNot(c => c == max).maxBy(c => c.n)
        val distance = max.n - second.n
        if (distance > totalVisits){
          node
        }
        else {
          searchNTimes(root_state, select(root_state, node)._1, n - 1, totalVisits = totalVisits - 1)

        }
      } else{
        searchNTimes(root_state, select(root_state, node)._1, n - 1, totalVisits = totalVisits - 1)

      }
    }
  }

  def getMCTSFunction: (Baduk, Int) => Move = {
    def MCTS(game: Baduk, visits: Int): Move = {
      val root = MCTSNode(game.null_move(), game.get_turn * -1, List(), Policy(game).moves.toList)
      val searchedRoot = searchNTimes(game, root, visits, totalVisits = visits)
      //for(c <- searchedRoot.children) println(s"Move ${c.move}: ${c.q} / ${c.n}")
      val best_child = searchedRoot.children.maxBy(_.n)
      println(s"Move ${best_child.move}: ${best_child.q} / ${best_child.n} (${(best_child.q / best_child.n * 100).round}%)")
      best_child.move
    }

    MCTS
  }

  def getMCTSRootNodeAndMaxFunction: (Baduk, Int) => (MCTSNode, Move) = {
    def MCTS(game: Baduk, visits: Int): (MCTSNode, Move) = {
      val root = MCTSNode(game.null_move(), game.get_turn * -1, List(), Policy(game).moves.toList)
      val searchedRoot = searchNTimes(game, root, visits, totalVisits = visits)
      //for(c <- searchedRoot.children) println(s"Move ${c.move}: ${c.q} / ${c.n}")
      val best_child = searchedRoot.children.maxBy(_.n)
      println(s"Move ${best_child.move}: ${best_child.q} / ${best_child.n} (${(best_child.q / best_child.n * 100).round}%)")
      (searchedRoot, best_child.move)
    }

    MCTS
  }

  def getContinuesMCTSRootNodeAndMaxFunction(rootTemp: MCTSNode, game: Baduk, visits: Int, totalVisits: Int): (MCTSNode, Move) = {
    val root =
      if (rootTemp == null)
        MCTSNode(game.null_move(), game.get_turn * -1, List(), Policy(game).moves.toList)
      else
        rootTemp
    val searchedRoot = searchNTimes(game, root, visits, totalVisits = totalVisits)
    //for(c <- searchedRoot.children) println(s"Move ${c.move}: ${c.q} / ${c.n}")
    val best_child = searchedRoot.children.maxBy(_.n)
    println(s"Move ${best_child.move}: ${best_child.q} / ${best_child.n} (${(best_child.q / best_child.n * 100).round}%)")
    (searchedRoot, best_child.move)
  }
}
