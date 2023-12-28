package AI.Heuristic


// 만약 낮은 레벨의 연결이 성립하지 않는다면 높은 레벨의 연결도 성립하지 않는다.
// strength 1이면 확실한 연결 0.5면 두개일 때 확실한 연결
case class Connect(dx: Int, dy: Int, strength: Float, level: Int)

object Connect {
  val IP_GU_JA: Connect = Connect(1, 1, strength = 0.8, level=0)

  // 한 칸
  val ONE_SPACE_JUMP: Connect = Connect(2, 0, strength = 0.5, level=1)

  val NAL_IL_JA_1: Connect = Connect(2, 1, strength = 0.5, level=2)
  val NAL_IL_JA_2: Connect = Connect(1, 2, strength = 0.5, level=2)

  // 두칸
  val TWO_SPACE_JUMP: Connect = Connect(3, 0, strength = 0.3, level=2)

  val NUN_MOK_JA_1: Connect = Connect(3, 1, strength = 0.3, level=3)
  val NUN_MOK_JA_2: Connect = Connect(1, 3, strength = 0.3, level=3)

  val CONNECTS_BY_DIRECTION: List[List[Connect]] = List(List(IP_GU_JA, ONE_SPACE_JUMP))


  val DENSE_CONNECTS: List[Connect] = (List(
    IP_GU_JA
  ).flatMap(c => List(
    c.copy(),
    c.copy(dx = -c.dx),
    c.copy(dy = -c.dy),
    c.copy(dx = -c.dx, dy = -c.dy)
  )) ::: List(
    Connect(1, 0, strength = 0.5, level=0), ONE_SPACE_JUMP
  ).flatMap(c => List(
    c.copy(),
    c.copy(dx = -c.dx),
    c.copy(dx = c.dy, dy = c.dx),
    c.copy(dx = -c.dy, dy = -c.dx)
  ))).sortBy(c => -c.level)

  val CONNECTS: List[Connect] = (List(
    IP_GU_JA, NAL_IL_JA_1, NAL_IL_JA_2, NUN_MOK_JA_1, NUN_MOK_JA_2
  ).flatMap(c => List(
    c.copy(),
    c.copy(dx = -c.dx),
    c.copy(dy = -c.dy),
    c.copy(dx = -c.dx, dy = -c.dy)
  )) ::: List(
    ONE_SPACE_JUMP, TWO_SPACE_JUMP
  ).flatMap(c => List(
    c.copy(),
    c.copy(dx = -c.dx),
    c.copy(dx = c.dy, dy = c.dx),
    c.copy(dx = -c.dy, dy = -c.dx)
  ))).sortBy(c => -c.strength)
}