package AI.PatternMatching

import AI.PatternMatching.Data
import Games.Baduk.Move

object DataParser {
  private val source = scala.io.Source.fromFile("Data.txt")
  private val lines: String = try source.mkString finally source.close()
  private def toMove(n: Int): Move = Move(n % 9, n / 9)
  val games: List[Data] = lines.split('\n').flatMap(s => {
    val items = s.split(' ')
    assert(items.head.length == 1)
    val winner = items.head.head
    val moves = items.tail.map(ns => toMove(ns.filter(c => c.isDigit).toInt)).toList
    generateTransformedGames(Data(moves, winner), 9)
  }).toList


  private def rotate90(data: Data, boardSize: Int): Data = {
    val rotatedMoves = data.moves.map { case Move(x, y) =>
      Move(boardSize - 1 - y, x)
    }
    Data(rotatedMoves, data.winner)
  }

  private def rotate180(data: Data, boardSize: Int): Data = {
    rotate90(rotate90(data, boardSize), boardSize)
  }

  private def rotate270(data: Data, boardSize: Int): Data = {
    rotate90(rotate180(data, boardSize), boardSize)
  }

  private def flipHorizontal(data: Data, boardSize: Int): Data = {
    val flippedMoves = data.moves.map { case Move(x, y) =>
      Move(x, boardSize - 1 - y)
    }
    Data(flippedMoves, data.winner)
  }

  private def flipVertical(data: Data, boardSize: Int): Data = {
    val flippedMoves = data.moves.map { case Move(x, y) =>
      Move(boardSize - 1 - x, y)
    }
    Data(flippedMoves, data.winner)
  }

  private def flipMainDiagonal(data: Data, boardSize: Int): Data = {
    val flippedMoves = data.moves.map { case Move(x, y) =>
      Move(y, x)
    }
    Data(flippedMoves, data.winner)
  }

  private def flipOtherDiagonal(data: Data, boardSize: Int): Data = {
    val flippedMoves = data.moves.map { case Move(x, y) =>
      Move(boardSize - 1 - y, boardSize - 1 - x)
    }
    Data(flippedMoves, data.winner)
  }

  def generateTransformedGames(originalGame: Data, boardSize: Int): List[Data] = {
    List(
      originalGame,
      rotate90(originalGame, boardSize),
      rotate180(originalGame, boardSize),
      rotate270(originalGame, boardSize),
      flipHorizontal(originalGame, boardSize),
      flipVertical(originalGame, boardSize),
      flipMainDiagonal(originalGame, boardSize),
      flipOtherDiagonal(originalGame, boardSize)
    )
  }
}
