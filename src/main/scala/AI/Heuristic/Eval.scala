package AI.Heuristic

import Games.Baduk.{Baduk, Move, Group}

import scala.collection.mutable
import scala.collection.immutable
import java.time.LocalDateTime
/*
* 1. 연결 그룹 묶기
* 2. 인접 그룹의 활로를 더하고  각각 돌의 활로를 정함
* 3. 각 돌의 영향력을 바둑판에 뿌림 (돌 위에 겹치게)
* 4. 각 칸당 최대 영향력에 제한을 걸음. (로그 함수 또는 단순 min(x, limit))
* 5. 영향력 합산
*  */

class Eval(baduk: Baduk) {
  private def effectFunc(effect: Float): Float = 90 - 90 / Math.pow(effect, 0.1f).toFloat
  private def effectForDistance(distance: Float) = 1 / distance
  private def effectForEachDistance(dx: Int, dy: Int) = effectForDistance(Math.sqrt(dx * dx + dy * dy).toFloat)
  private def positionBonus(x: Int, y: Int) = 1
  /*
    1 +
      (if (x < size / 2) 1 / (x + 1f) else 1 / (9f - x)) +
        (if (y < size / 2) 1 / (y + 1f) else 1 / (9f - y))*/

  private def effectSumFunc(effect: Float): Float =
    /*if (effect> 4.2f) 1f
    else if (effect < -4.2f) -1f
    else 0*/

    if(effect > 0) 1 - 1 / Math.max(1f, Math.sqrt(effect).toFloat)
    else -(1 - 1 / Math.sqrt(Math.max(1f, -effect)).toFloat)

  private val effectLimit: Int = 3  // 좌우로 -3 +3 -> 7칸 -> 49 - 1 칸


  val size: Int = baduk.get_board.size
  val get: Move => Int = baduk.get_board.get

  private val keyToGroupMap: mutable.HashMap[Move, Group] = mutable.HashMap()
  val groups: mutable.ListBuffer[Group] = baduk.get_board.collectGroups
  val lives: Array[Float] = Array.fill(size * size)(0f)
  val effects: Array[Float] = Array.fill(size * size)(0f)

  private def fromTo(from: Int, to: Int): Seq[Int] =
    if (from <= to)
      from to to
    else
      to to from

  private def connectGroups(): Unit = {
    val boardMap: Array[Group] = Array.fill[Group](size * size)(null)

    // map board
    for (group <- groups) {
      for (stone <- group.stones) {
        boardMap(stone.x + stone.y * size) = group
      }
    }

    // connect groups
    for (x <- 0 until size; y <- 0 until size; pos = Move(x, y); if get(pos) != Baduk.EMPTY) {
      val color = get(pos)
      val thisGroup = boardMap(x + y * size)

      var failedLevel = 1000
      for (connect <- Connect.CONNECTS/*; if connect.level <= failedLevel*/) {
        //println(connect)
        val x_condition = if(connect.dx > 0) x + connect.dx < size else x + connect.dx >= 0
        val y_condition = if(connect.dy > 0) y + connect.dy < size else y + connect.dy >= 0
        var obstacles = 0

        if (x_condition && y_condition) {
          var isConnected = true
          if (get(Move(x + connect.dx, y + connect.dy)) == color && boardMap(Move(x + connect.dx, y + connect.dy).toNumber(size)) != thisGroup) {
            for (dx <- fromTo(0, connect.dx); dy <- fromTo(0, connect.dy); if isConnected)
              if (!(dx == 0 && dy == 0) && !(dx == connect.dx && dy == connect.dy) && get(Move(x + dx, y + dy)) == color * -1) {
                if (connect == Connect.IP_GU_JA){
                  if (obstacles == 0)
                    obstacles +=1
                  else
                    isConnected = false
                } else{
                  isConnected = false

                }
              }
          }
          else {
            isConnected = false
          }

          if (isConnected) {
            val next = Move(x + connect.dx, y + connect.dy).toNumber(size)
            val nextGroup = boardMap(next)

            val strength = connect.strength / 2 * ((3 - obstacles).toFloat / 3f)

            assert(thisGroup != nextGroup)
            thisGroup.connectedGroups.get(nextGroup.stones.head) match
              case Some(st: Float) => {
                thisGroup.connectedGroups(nextGroup.stones.head) += strength
                nextGroup.connectedGroups(thisGroup.stones.head) += strength
              }
              case _ =>{
                thisGroup.connectedGroups(nextGroup.stones.head) = strength
                nextGroup.connectedGroups(thisGroup.stones.head) = strength
              }
          }
        }
      }
    }

    // generate key to group map
    for (group <- groups) {
      keyToGroupMap += (group.stones.head -> group)
    }

    // strength 가 1 이상으로 연결되면 합친다.
    var stop = false
    while (!stop) {
      var i = 0
      stop = true
      while (i < groups.length) {
        for ((key, strength) <- groups(i).connectedGroups) {
          if (strength >= 1f) {
            // merge
            val groupToMerge = keyToGroupMap(key)
            val index = groups.indexOf(groupToMerge)
            val merged = groups(i).merge(groupToMerge)
            groups.update(i, merged)
            for (s <- groups(i).stones ++ groupToMerge.stones)
              keyToGroupMap(s) = merged

            groups -= groupToMerge
            if (index < i)
              i -= 1
            stop = false
          }
        }
        i += 1
      }
    }

    //println(groups.mkString("Array(", ",\n ", ")"))
  }

  private def distributeLives(): Unit = {
    val livesMap = mutable.HashMap[Group, Float]()
    for (group <- groups) {
      livesMap(group) = 0f
      for (stone <- group.stones) {
        keyToGroupMap(stone) = group
      }
    }
    for (group <- groups) {
      val deltaMap = mutable.HashMap[Group, Float]()
      def dfs(node: Group, value: Float): Unit = {
        if (deltaMap.contains(node) && deltaMap(node) >= value) {
          return
        }
        deltaMap(node) = value
        for((key, weight) <- node.connectedGroups) {
          dfs(keyToGroupMap(key), value * weight)
        }
      }
      dfs(group, group.lives)
      for ((k, v) <- deltaMap) {
        livesMap(k) += v
      }
    }

    for (group <- groups) {
      val finalLiveEffect = effectFunc(livesMap(group))
      for (stone <- group.stones) {
        val n = stone.toNumber(size)
        lives(n) = finalLiveEffect
      }
    }
    //println(livesMap)
    //println(lives.mkString("Array(", ", ", ")"))
  }

  // for black
  private def distributeEffects(): Unit = {
    def range(p: Int) = Math.max(0, p - effectLimit) to Math.min(size-1, p + effectLimit)
    for (px <- 0 until size; py <- 0 until size; pos = Move(px, py); if get(pos) != Baduk.EMPTY) {
      val livesAtPosition = lives(px + py * size)
      val color = get(pos)
      for (x <- range(px); y <- range(py)) {
        val effect = effectForEachDistance(x - px, y - py) * livesAtPosition * positionBonus(x, y)
        if (color == Baduk.BLACK)
          effects(x + y * size) += effect
        else
          effects(x + y * size) -= effect
      }
    }
    for (i <- 0 until size * size)
      effects(i) = effectSumFunc(effects(i))
    //println(effects.mkString("Effects(", ", ", ")"))
  }


  def getEffects: Array[Float] = {
    connectGroups()
    distributeLives()
    distributeEffects()
    effects
  }

  def valueByBlack: Float = {
    val t0 = System.nanoTime()
    connectGroups()
    distributeLives()
    distributeEffects()
    //println((System.nanoTime() - t0) / Math.pow(10, 9))
    var sum = 0f
    for (i <- 0 until size * size if baduk.get_board.at(i) == Baduk.EMPTY)
      sum += effects(i)
    sum - baduk.get_komi + (baduk.blackCaptures - baduk.whiteCaptures) * 1
  }
  def valueByWhite: Float = -valueByBlack
  def valueBy(color: Int): Float = color match
    case Baduk.BLACK => valueByBlack
    case Baduk.WHITE => valueByWhite
    case _ => throw new Exception("?")

  def getWinner: Int = if (valueByBlack > 0) Baduk.BLACK else Baduk.WHITE
}
