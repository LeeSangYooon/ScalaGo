package AI.Heuristic

import AI.PatternMatching.DataParser
import Games.Baduk.Move

import scala.collection.mutable
import scala.util.Random

case class BookNode(children: mutable.HashMap[Move, BookNode]) {
  def has(move: Move): Boolean = children.contains(move)
  def go(move: Move): BookNode =
    if (has(move)) children(move)
    else null

  def goRandom(): (Move, BookNode) = {
    val randomIndex = Random.nextInt(children.size)
    val list = children.toList
    val randomKeyValuePair = list(randomIndex)

    randomKeyValuePair

  }
}

object BookNode {
  val root: BookNode = {
    val tempRoot = BookNode(mutable.HashMap())
    for (gameData <- DataParser.games) {
      var top = tempRoot
      for (move <- gameData.moves) {
        if (top.children.contains(move)) {
          top = top.children(move)
        }
        else {
          val newNode = BookNode(mutable.HashMap())
          top.children += (move -> newNode)
          top = newNode
        }
      }
    }
    tempRoot
  }
}
