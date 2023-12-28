package GUI

import AI.Heuristic.{BookNode, Eval, Policy}
import AI.MCTS.MCTS
import AI.PatternMatching.{DataParser, PatternMatching}
import Games.Baduk.{Baduk, Move}

import java.awt.{Color, Dimension, Graphics2D}
import scala.swing.*
import scala.swing.event.*

object GoBoardGUI extends SimpleSwingApplication {

  val boardSize = 9
  private val cellSize = 50
  private val margin = 20
  private val baduk = Baduk()
  private val pattern = PatternMatching(DataParser.games.slice(0, 1), 3)
  private var bookNode = BookNode.root


  private def think(b: Baduk) : Move = {
    if (bookNode != null) {
      val (move, node) = bookNode.goRandom()
      bookNode = node
      move
    }
    else {
      val mcts = MCTS.getMCTSRootNodeAndMaxFunction
      val (root, move) = mcts(b, 20000)
      //println(f"black ${Eval(b.go(move)).valueByBlack}")
      println(root.children)

      move
    }
  }

  val restartButton = new Button("재시작")
  val progressBar = new ProgressBar {
    min = 0
    max = 100
    value = 0
    foreground = new Color(250,100, 0) // Set custom color (green)
  }
  def top: MainFrame = new MainFrame {
    title = "알파고"
    contents = new BorderPanel {
      layout(restartButton) = BorderPanel.Position.North
      layout(progressBar) = BorderPanel.Position.South
      layout(new BoardPanel(boardSize, cellSize, margin, baduk,
        blackCallBack = (b: Baduk, x: Int, y: Int) => {
          val move = Move(x, y)
          println(f"black ${Eval(b.go(move)).valueByBlack}")
          if (bookNode != null)
            bookNode = bookNode.go(move)
          b.go(Move(x, y))
        },
        whiteCallBack = (b: Baduk, x: Int, y: Int) => {
          b


        },
        keyCallBack = (b: Baduk, k: Key.Value) => {
          Baduk()
        },
        mapper = (b: Baduk) => {
          pattern.evaluate(b)
        },
        whiteAI = true,
        ai = think,
        restartButton = restartButton,
        progressBar = progressBar
      )) = BorderPanel.Position.Center
    }

    centerOnScreen()
  }
}

class BoardPanel(boardSize: Int, cellSize: Int, margin: Int, badukFirst: Baduk,
                 blackCallBack: (Baduk, Int, Int) => Baduk, whiteCallBack: (Baduk, Int, Int) => Baduk,
                 keyCallBack: (Baduk, Key.Value) => Baduk, mapper: Baduk => Array[Float] ,
                 blackAI: Boolean = false, whiteAI: Boolean = false, ai: Baduk => Move = null,
                 aiVisits: Int = 30000, restartButton: Button = null, progressBar: ProgressBar) extends Panel {

  private val boardWidth = (boardSize - 1) * cellSize
  private val totalWidth = boardWidth + 2 * margin
  private var baduk = badukFirst
  private var mapperToggle = false
  private var effectToggle = false
  private var mctsToggle = true
  private var doAI: Int = -1
  private var mctsRoot: MCTS.MCTSNode = null
  private var toMove: Move = null
  private var bookNode = BookNode.root
  preferredSize = new Dimension(totalWidth, totalWidth)

  private var lastClicked: Option[(Int, Int)] = None

  listenTo(mouse.clicks, keys, restartButton)


  reactions += {
    case MousePressed(_, point, _, _, _) =>
      lastClicked = getClickedIntersection(point)
      requestFocus()
      repaint()

    case ButtonClicked(`restartButton`) =>
      //  RESTART!
      println("재시작")
      baduk = badukFirst
      doAI = -1
      mctsRoot = null
      toMove = null
      bookNode = BookNode.root

    case KeyPressed(_, Key.T, _, _) =>
      mapperToggle = !mapperToggle
      repaint()
    case KeyPressed(_, Key.E, _, _) =>
      effectToggle = !effectToggle
      repaint()
    case KeyPressed(_, Key.M, _, _) =>
      mctsToggle = !mctsToggle
      repaint()
    case KeyPressed(_, k, _, _) =>
      baduk = keyCallBack(baduk, k)
      requestFocus()
      repaint()

  }

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    drawBoard(g)
  }

  private def drawBoard(g: Graphics2D): Unit = {
    if (doAI > 0) {
      if (bookNode != null) {
        val (move, node) = bookNode.goRandom()
        bookNode = node
        baduk = baduk.go(move)
        doAI = -1
      }
      else{
        val (node, move) = MCTS.getContinuesMCTSRootNodeAndMaxFunction(mctsRoot, baduk, 100, doAI)
        mctsRoot = node
        toMove = move

        doAI -= 100
        progressBar.value = (aiVisits - doAI) * 100 / aiVisits
        if (doAI <= 0) {
          baduk = baduk.go(toMove)
          mctsRoot = null
          lastClicked = Some((toMove.y, toMove.x))
        }
      }

    }
    else {
      if (blackAI && baduk.get_turn == Baduk.BLACK) {
        doAI = aiVisits
      }
      if (whiteAI && baduk.get_turn == Baduk.WHITE) {
        doAI = aiVisits

      }
    }
    repaint()

    // Draw background
    g.setColor(new Color(3,141,6))
    g.fillRect(0, 0, size.width, size.height)

    // Draw board background
    g.setColor(new Color(255, 230, 132))
    g.fillRect(margin, margin, boardWidth, boardWidth)

    // Draw grid lines
    g.setColor(Color.BLACK)
    for (i <- 0 until boardSize) {
      val linePos = margin + i * cellSize
      g.drawLine(linePos, margin, linePos, margin + boardWidth)
      g.drawLine(margin, linePos, margin + boardWidth, linePos)
    }

    // Draw star points (9x9 and 19x19 only)
    if (boardSize == 9 || boardSize == 19) {
      val drawStar = (x: Int, y: Int) => {
        val starX = margin + x * cellSize
        val starY = margin + y * cellSize
        g.fillOval(starX - 5, starY - 5, 10, 10)
      }
      for (i <- 2 to 6 by 4; j <- 2 to 6 by 4) {
        drawStar(i, j)
      }
      drawStar(boardSize / 2, boardSize / 2)
    }

    // Draw stones
    for (x <- 0 until boardSize; y <- 0 until boardSize; if baduk.get_board.get(Move(x, y)) != Baduk.EMPTY) {
      if (baduk.get_board.get(Move(x, y)) == Baduk.BLACK)
        g.setColor(Color.BLACK)
      else
        g.setColor(Color.WHITE)
      val stoneX = margin + x * cellSize
      val stoneY = margin + y * cellSize
      val stoneSize = 46
      g.fillOval(stoneX - stoneSize / 2, stoneY - stoneSize / 2, stoneSize, stoneSize)
    }

    // Draw last clicked intersection
    lastClicked.foreach { case (row, col) =>
      g.setColor(Color.RED)
      val x = margin + col * cellSize
      val y = margin + row * cellSize
      g.fillOval(x - 5, y - 5, 10, 10)
    }

    /*
    if (mapper != null && mapperToggle) {
      val map = mapper(baduk)
      val max = map.max
      for (x <- 0 until boardSize; y <- 0 until boardSize;
           if baduk.get_moves().contains(Move(x, y));
           value = map(x + y * boardSize) / max ;
           if value > 0.4f) {

        g.setColor(new Color(0, value * 0.8f, value, value))
        val stoneX = margin + x * cellSize
        val stoneY = margin + y * cellSize
        val stoneSize = 46
        g.fillOval(stoneX - stoneSize / 2, stoneY - stoneSize / 2, stoneSize, stoneSize)
      }
    }*/
    if (mapperToggle) {
      val policy = Policy(baduk).moves
      for (x <- 0 until boardSize; y <- 0 until boardSize;
           if policy.contains(Move(x, y))) {

        g.setColor(new Color(0, 1 * 0.8f, 1, 0.7f))
        val stoneX = margin + x * cellSize
        val stoneY = margin + y * cellSize
        val stoneSize = 46
        g.fillOval(stoneX - stoneSize / 2, stoneY - stoneSize / 2, stoneSize, stoneSize)
      }
    }

    if (effectToggle) {
      val map = Eval(baduk).getEffects
      val max = 1
      for (x <- 0 until boardSize; y <- 0 until boardSize
           /*if baduk.get_moves().contains(Move(x, y)*/;
           value = map(x + y * boardSize) / max) {

        if (value > 0) {
          g.setColor(new Color(0, 0, 0, value))
        }
        else {
          g.setColor(new Color(1, 1, 1, -value))
        }


        val stoneX = margin + x * cellSize
        val stoneY = margin + y * cellSize
        val stoneSize = 46
        g.fillRect((stoneX - stoneSize / 6).toInt, (stoneY - stoneSize / 6).toInt, stoneSize / 3, stoneSize /3)
      }
    }

    if (mctsToggle && mctsRoot != null) {
      val map = Array.fill(boardSize * boardSize)(0f)
      val rateMap = Array.fill(boardSize * boardSize)(0f)

        for (child <- mctsRoot.children) {
        if (0 <= child.move.toNumber(boardSize))
        map(child.move.toNumber(boardSize)) = child.n.toFloat
        rateMap(child.move.toNumber(boardSize)) = child.q / child.n.toFloat
      }
      val max = map.max
      for (x <- 0 until boardSize; y <- 0 until boardSize;
           if baduk.get_moves().contains(Move(x, y));
           value = map(x + y * boardSize) / max;
           if value > 0.4f) {

        g.setColor(new Color(0, value * 0.8f, value, value))
        val stoneX = margin + x * cellSize
        val stoneY = margin + y * cellSize
        val stoneSize = 46
        g.fillOval(stoneX - stoneSize / 2, stoneY - stoneSize / 2, stoneSize, stoneSize)
        g.setColor(Color.BLACK)

        g.setFont(new Font("TimesRoman", 0, 17));
        val str= f"${(rateMap(x + y * boardSize) * 100).toInt}" ++ "%"
        g.drawString(str, stoneX- 10 * str.length / 2, stoneY + 5)
      }
    }


  }

  private def getClickedIntersection(point: Point): Option[(Int, Int)] = {
    val col = ((point.x - margin).toFloat / cellSize.toFloat).round
    val row = ((point.y - margin).toFloat / cellSize.toFloat).round

    if (col >= 0 && col < boardSize && row >= 0 && row < boardSize && baduk.get_moves().contains(Move(col, row))) {
      if (baduk.get_turn == Baduk.BLACK)
        baduk = blackCallBack(baduk, col, row)
      else
        baduk = whiteCallBack(baduk, col, row)


      if (bookNode != null)
        bookNode = bookNode.go(Move(col, row))
      Some((row.toInt, col.toInt))
    } else
      None
  }
}
