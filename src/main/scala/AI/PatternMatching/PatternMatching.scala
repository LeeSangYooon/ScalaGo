package AI.PatternMatching

import Games.Baduk.{Baduk, Move}

import scala.collection.mutable
import scala.collection.mutable.HashMap
import java.io.{ObjectOutputStream, FileOutputStream}
import java.io.{ObjectInputStream, FileInputStream}

// pattern match for black
class PatternMatching(dataset: List[Data], chunkSize: Int = 4) {
  var dict: mutable.HashMap[Seq[Int], (Int, List[Int])] = mutable.HashMap()

  private def calculate() : Unit = {
    println(s"calculating...(${dataset.length})")
    dataset.zipWithIndex foreach {
      case (data, count) =>
        print(f"${count} ")
        var game = Baduk()
        for (move <- data.moves.dropRight(1); next_move <- data.moves.tail) {
          //scan
          val outOfBounds = (n: Int, size: Int) => n < 0 || n >= size
          for (i <- -1 to 10 - chunkSize; j <- -1 to 10 - chunkSize) {
            val key =
              for (x <- j until j + chunkSize; y <- i until i + chunkSize)
                yield
                  if (outOfBounds(x, 9) || outOfBounds(y, 9)) -2
                  else game.get_board.get(Move(x, y)) * (if (game.get_turn == Baduk.BLACK) 1 else -1)

            val nextMoveInChunk = Move(next_move.x - j, next_move.y - i)
            if (outOfBounds(nextMoveInChunk.x, chunkSize) || outOfBounds(nextMoveInChunk.y, chunkSize)) {
              dict.get(key) match
                case Some(value) => dict(key) = (value._1 + 1, value._2)
                case None => dict += (key -> (1, List.fill(chunkSize * chunkSize)(0)))
            }
            else {
              val moveIndex = nextMoveInChunk.x + nextMoveInChunk.y * chunkSize
              dict.get(key) match
                case Some(value) => dict(key) = (value._1 + 1, value._2.updated(moveIndex, value._2(moveIndex) + 1))
                case None => dict += (key -> (1, List.fill(chunkSize * chunkSize)(0).updated(moveIndex, 1)))
            }

          }
          game = game.go(move)
        }
    }
    println("")
  }
  private def save(): Unit = {
    println("saving patterns...")
    // Specify the file path
    val filePath = "hashmap.ser"

    // Save the HashMap to a file
    saveHashMapToFile(dict, filePath)

    // Function to save HashMap to a file
    def saveHashMapToFile(hashMap: scala.collection.mutable.HashMap[Seq[Int], (Int, List[Int])], path: String): Unit = {
      val fileOutputStream = new FileOutputStream(path)
      val objectOutputStream = new ObjectOutputStream(fileOutputStream)

      try {
        objectOutputStream.writeObject(hashMap)
      } finally {
        objectOutputStream.close()
        fileOutputStream.close()
      }
    }
  }
  private def load(): Unit = {
    println("loading patterns... ")
    val filePath = "hashmap.ser"
    // Function to load HashMap from a file
    def loadHashMapFromFile(path: String): scala.collection.mutable.HashMap[Seq[Int], (Int, List[Int])] = {
      val fileInputStream = new FileInputStream(path)
      val objectInputStream = new ObjectInputStream(fileInputStream)

      try {
        objectInputStream.readObject().asInstanceOf[scala.collection.mutable.HashMap[Seq[Int], (Int, List[Int])]]
      } finally {
        objectInputStream.close()
        fileInputStream.close()
      }
    }

    // Load the HashMap from the file
    dict = loadHashMapFromFile(filePath)
  }
  {
    calculate()
    //save()
    //load()
    println("done")
  }
  private def eval(child: Int, parent: Int): Float = Math.pow(child.toFloat / Math.max(parent, 2f), 0.5).toFloat
  private def toMove(n: Int): Move = Move(n % 9, n / 9)

  def evaluate(baduk: Baduk): Array[Float] = {
    val outOfBounds = (n: Int, size: Int) => n < 0 || n >= size
    val boardOutput: Array[Float] = Array.fill(81)(0)
    for (i <- -1 to 10 - chunkSize; j <- -1 to 10 - chunkSize) {
      val key =
        for (x <- j until j + chunkSize; y <- i until i + chunkSize)
          yield
            if (outOfBounds(x, 9) || outOfBounds(y, 9)) -2
            else baduk.get_board.get(Move(x, y)) * (if (baduk.get_turn == Baduk.BLACK) 1 else -1)


      dict.get(key) match
        case Some(value) =>
          for (dx <- 0 until chunkSize; dy <- 0 until chunkSize if !outOfBounds(j + dx, 9) && !outOfBounds(i + dy, 9)) {
            boardOutput((j + dx) + 9 * (i + dy)) += eval(value._2(dx + chunkSize * dy), value._1)
          }
        case _ =>
    }
    boardOutput
  }

  def predict(baduk: Baduk): Move = {
    val boardOutput = evaluate(baduk)
    println(boardOutput.mkString("Array(", ", ", ")"))

    var max = -1f
    var maxMove = Move(-1, -1)
    for (x <- 0 until 9 ; y <- 0 until 9; move = Move(x, y); if baduk.get_moves().contains(move)) {
      val value = boardOutput(x + y * 9)
      if (value > max) {
        max = value
        maxMove = move
      }
    }
    maxMove
  }
}
