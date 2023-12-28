
import AI.MCTS.PureMCTS
import AI.PatternMatching.{DataParser, PatternMatching}
import GUI.GoBoardGUI
import Games.Baduk.Baduk

import scala.annotation.tailrec

@main
def main(): Unit = {
  /*
  var test = Baduk.Baduk.newGame(size = 4)
  badukTest()*/
  //baduk_play(PureMCTS.getMCTSFunction[Baduk.Move](), Baduk.Baduk.newGame(7))
  //GoBoardGUI.main(Array())
 // println("hi")
  val dataset = DataParser.games.slice(0,200)
  val pattern = PatternMatching(dataset, 3)
  var game = Baduk()
  while (!game.end()) {
    val (root, move) = PureMCTS(pattern).MCTS(game, 1)
    game.show()
    game = game.go(move)
  }

  /*
  val dataset = DataParser.games.slice(0,400)
  val pattern = PatternMatching(dataset, 4)
  var game = Baduk.Baduk()
  for(n <- 0 until 50) {
    val move = pattern.predict(game)
    println(game)
    println(move)
    game = game.go(move)
  }*/

}