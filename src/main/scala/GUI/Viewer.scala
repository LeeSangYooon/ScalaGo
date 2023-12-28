package GUI

import AI.PatternMatching.{DataParser, PatternMatching}
import Games.Baduk.{Baduk, Move}

import java.awt.{Color, Dimension, Graphics2D}
import scala.swing.*
import scala.swing.MenuBar.NoMenuBar.requestFocus
import scala.swing.event.*

object Viewer extends SimpleSwingApplication {

  val boardSize = 9
  private val cellSize = 50
  private val margin = 20
  private val baduk = Baduk()

  private val dataset = DataParser.games
  private var data = dataset.head
  private var moves = 0
  private var index = 0
  private val pattern = PatternMatching(DataParser.games.slice(0,1), 4)

  def top: MainFrame = new MainFrame {
    title = "Game Viewer: data 0"
    contents = new BoardPanel(boardSize, cellSize, margin, baduk,
      blackCallBack = (b: Baduk, x: Int, y: Int) => {
        b
      },
      whiteCallBack = (b: Baduk, x: Int, y: Int) => {
        b
      }, keyCallBack = (b: Baduk, k: Key.Value) => {
        k match
          case Key.Space => {
            if (moves < data.moves.length) {
              moves += 1
              b.go(data.moves(moves - 1))
            } else
              b
          }
          case Key.Right => {
            index = (index + 1) % dataset.length
            title = f"Game Viewer: data ${index}"
            moves = 0
            data = dataset(index)
            Baduk()
          }
          case Key.Left => {
            index = (index - 1 + dataset.length) % dataset.length
            title = f"Game Viewer: data ${index}"
            moves = 0
            data = dataset(index)
            Baduk()
          }
      },
      mapper = (b: Baduk) => {
        pattern.evaluate(b)
      },
      progressBar = null)
    centerOnScreen()
    requestFocus()
  }
}
