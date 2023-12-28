package AI.Heuristic

import Games.Baduk.{Baduk, Move}


class Policy(baduk: Baduk) {
  private val size = baduk.get_board.size
  def moves: Seq[Move] = {
    val board = Array.fill(size * size)(false)
    for (x <- 0 until size; y <- 0 until size; move = Move(x, y); if baduk.get_board.get(move) != Baduk.EMPTY) {
      for (connect <- Connect.DENSE_CONNECTS; pos = Move(connect.dx + x, connect.dy + y)) {
        if (0 <= pos.x && pos.x < size && 0 <= pos.y && pos.y < size){
          board(pos.x + pos.y * size) = true
        }
      }
    }
    for (n <- 0 until size * size; move = Move(n % size, n / size) ; if board(n) && baduk.get_moves().contains(move))
      yield move


  }
}
