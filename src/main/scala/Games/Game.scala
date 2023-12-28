package Games
// T: Move
trait Game[T] {
  def get_turn: Int
  def get_moves(): List[T]
  def go(move: T): Game[T]
  def end(): Boolean
  def winner(): (Int, Int)
  def null_move(): T
  def to_vector(for_whose_turn: Int): Array[Double]
  def get_best_moves(array: Array[Double], number: Int): List[T]
  def turn_switched(): Game[T]
}
