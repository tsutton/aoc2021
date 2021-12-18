package day18

enum Node:
  def reduce(): Unit =
    if explode() then
      // println("exploded")
      reduce()
    else if split() then
      // println("split")
      reduce()

  def magnitude: Int = this match
    case Leaf(i)               => i
    case Pair(InnerNode(l, r)) => 3 * l.magnitude + 2 * r.magnitude

  private def split(): Boolean = ???

  private def explode(): Boolean = explodeAtDepth(0)._1

  private def explodeAtDepth(depth: Int): (Boolean, Option[Int], Option[Int]) =
    this match
      case l: Leaf => (false, None, None)
      case p: Pair => p.node.explodeAtDepth(depth)

  def pushRight(value: Int): Unit = this match
    case i: Leaf => i.value = i.value + value
    case p: Pair => p.node.right.pushRight(value)

  def pushLeft(value: Int): Unit = this match
    case i: Leaf => i.value = i.value + value
    case p: Pair => p.node.left.pushLeft(value)

  def pushRightInLeftSubtree(value: Int) = this match
    case i: Leaf => i.value = i.value + value
    case p: Pair => p.node.left.pushRight(value)

  def pushLeftInRightSubtree(value: Int): Unit = this match
    case i: Leaf => i.value = i.value + value
    case p: Pair => p.node.right.pushLeft(value)

  case Pair(var node: InnerNode)
  case Leaf(var value: Int)

case class InnerNode(var left: Node, var right: Node):
  def explodeAtDepth(depth: Int): (Boolean, Option[Int], Option[Int]) =
    if depth == 3 then
      left match
        case Node.Pair(InnerNode(Node.Leaf(a), Node.Leaf(b))) => {
          left = Node.Leaf(0)
          right.pushLeft(b)
          return (true, Some(a), None)
        }
        case Node.Pair(_) =>
          throw RuntimeException("exploding found unreduced node")
        case _ => {} // try right
      right match
        case Node.Pair(InnerNode(Node.Leaf(a), Node.Leaf(b))) => {
          right = Node.Leaf(0)
          left.pushRight(b)
          return (true, None, Some(b))
        }
        case Node.Pair(_) =>
          throw RuntimeException("exploding found unreduced node")
        case _ => return (false, None, None) // both children were leaves
    end if
    left match
      case p: Node.Pair =>
        p.node.explodeAtDepth(depth + 1) match
          case (true, Some(a), x) => return (true, Some(a), x)
          case (true, x, Some(a)) => {
            p.pushLeftInRightSubtree(a)
            return (true, None, None)
          }
          case (true, x, y) => return (true, x, y)
          case _            => {}
      case _: Node.Leaf => {}
    right match
      case p: Node.Pair =>
        p.node.explodeAtDepth(depth + 1) match
          case (true, x, Some(a)) => return (true, x, Some(a))
          case (true, Some(a), x) => {
            p.pushRightInLeftSubtree(a)
            return (true, None, None)
          }
          case (true, x, y) => return (true, x, y)
          case _            => {}
      case _: Node.Leaf => {}
    return (false, None, None)

def parseNode(source: String, startIdx: Int): (Node, Int) =
  // println(s"parsing pair in ${source.substring(startIdx)}")
  assert(source(startIdx) == '[')
  var currentIdx = startIdx + 1
  val left: Node = source(currentIdx) match
    case '[' => {
      val (leftTmp, currentTmp) = parseNode(source, currentIdx)
      assert(source(currentTmp) == ',')
      currentIdx = currentTmp + 1
      leftTmp
    }
    case _ => {
      val commaIdx = source.indexOf(',', currentIdx)
      // println(s"parsing int from ${source.substring(currentIdx, commaIdx)}")
      val leftTmp = Integer.parseInt(source.substring(currentIdx, commaIdx))
      assert(source(commaIdx) == ',')
      currentIdx = commaIdx + 1
      Node.Leaf(leftTmp)
    }
  // println(
  //   s"got left=${left}, parsing right from ${source.substring(currentIdx)}"
  // )
  val right: Node = source(currentIdx) match
    case '[' => {
      val (leftTmp, currentTmp) = parseNode(source, currentIdx)
      assert(source(currentTmp) == ']')
      currentIdx = currentTmp + 1
      leftTmp
    }
    case _ => {
      val endIdx = source.indexOf(']', currentIdx)
      val leftTmp = Integer.parseInt(source.substring(currentIdx, endIdx))
      assert(source(endIdx) == ']')
      currentIdx = endIdx + 1
      Node.Leaf(leftTmp)
    }
  assert(source(currentIdx - 1) == ']')
  // println(s"got right=${right}")
  (Node.Pair(InnerNode(left, right)), currentIdx)
