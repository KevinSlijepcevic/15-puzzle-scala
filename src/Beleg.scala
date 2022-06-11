import java.time.LocalDateTime
import scala.annotation.tailrec

object Beleg extends App {
  val sortedList = List(Some(1), Some(2), Some(3), Some(4),
    Some(5), Some(6), Some(7), Some(8),
    Some(9), Some(10), Some(11), Some(12),
    Some(13), Some(14), Some(15), None)

  case class Node(value: List[Option[Int]], history: List[Char], depth: Int) {
    def isCorrect: Boolean = value.equals(sortedList)

    def nextMoves: List[Node] = {
      history match {
        case Nil =>
          List(
            Node(moveValueUp(value), history :+ 'u', depth + 1),
            Node(moveValueDown(value), history :+ 'd', depth + 1),
            Node(moveValueLeft(value), history :+ 'l', depth + 1),
            Node(moveValueRight(value), history :+ 'r', depth + 1)
          ).filter(n => n.value.nonEmpty)
        case _ =>
          if (history.last == 'd')
            List(
              Node(moveValueDown(value), history :+ 'd', depth + 1),
              Node(moveValueLeft(value), history :+ 'l', depth + 1),
              Node(moveValueRight(value), history :+ 'r', depth + 1)
            ).filter(n => n.value.nonEmpty)
          else if (history.last == 'u')
            List(
              Node(moveValueUp(value), history :+ 'u', depth + 1),
              Node(moveValueLeft(value), history :+ 'l', depth + 1),
              Node(moveValueRight(value), history :+ 'r', depth + 1)
            ).filter(n => n.value.nonEmpty)
          else if (history.last == 'l')
            List(
              Node(moveValueDown(value), history :+ 'd', depth + 1),
              Node(moveValueUp(value), history :+ 'u', depth + 1),
              Node(moveValueLeft(value), history :+ 'l', depth + 1),
            ).filter(n => n.value.nonEmpty)
          else
            List(
              Node(moveValueDown(value), history :+ 'd', depth + 1),
              Node(moveValueUp(value), history :+ 'u', depth + 1),
              Node(moveValueRight(value), history :+ 'r', depth + 1)
            ).filter(n => n.value.nonEmpty)
      }
    }

    def printPath(): Unit = println(history)

  }

  def moveValueLeft(value: List[Option[Int]]): List[Option[Int]] = {
    val emptyIndex = value.indexOf(None)
    if (emptyIndex % 4 == 0) List()
    else value.slice(0, emptyIndex - 1) ::: None :: value(emptyIndex - 1) :: value.slice(emptyIndex + 1, 16)
  }

  def moveValueRight(value: List[Option[Int]]): List[Option[Int]] = {
    val emptyIndex = value.indexOf(None)
    if (emptyIndex == 3 || emptyIndex == 7 || emptyIndex == 11 || emptyIndex == 15) List()
    else value.slice(0, emptyIndex) ::: value(emptyIndex + 1) :: None :: value.slice(emptyIndex + 2, 16)
  }

  def moveValueUp(value: List[Option[Int]]): List[Option[Int]] = {
    val emptyIndex = value.indexOf(None)
    if (emptyIndex <= 3) List()
    else value.slice(0, emptyIndex - 4) ::: None :: value.slice(emptyIndex - 3, emptyIndex) ::: value(emptyIndex - 4) :: value.slice(emptyIndex + 1, 16)
  }

  def moveValueDown(value: List[Option[Int]]): List[Option[Int]] = {
    val emptyIndex = value.indexOf(None)
    if (emptyIndex >= 12) List()
    else value.slice(0, emptyIndex) ::: value(emptyIndex + 4) :: value.slice(emptyIndex + 1, emptyIndex + 4) ::: None :: value.slice(emptyIndex + 5, 16)
  }

  def search(node: Node, bound: Int): (Node, Int) = {
    val f = calcHeuristicCost(node.value) + node.depth
    if (f > bound) (null, f)
    else if (node.isCorrect) (node, 0)
    else {
      val minInit = Integer.MAX_VALUE
      val nextNodes = node.nextMoves
      @tailrec
      def searchRec(nextNodes: List[Node], min: Int): (Node, Int) = {
        nextNodes match {
          case Nil => (null, min)
          case h::t =>
            val searchResult = search(h, bound)
            if (searchResult._1 != null) searchResult
            else if (searchResult._2 < min && searchResult._2 != 0) searchRec(t, searchResult._2)
            else searchRec(t, min)
        }
      }
      searchRec(nextNodes, minInit)
    }
  }

  def solvePuzzle(root: Node): Node = {
    val initBound = calcHeuristicCost(root.value)
    @tailrec
    def solvePuzzleRec(node: Node, bound: Int): Node = {
      val searchResult = search(node, bound)
      if (searchResult._1 != null) searchResult._1
      else solvePuzzleRec(node, searchResult._2)
    }
    solvePuzzleRec(root, initBound)
  }

  def calcHeuristicCost(value: List[Option[Int]]): Int = {
    @tailrec
    def calcHeuristicCostRek(value: List[Option[Int]], acc: Int, index: Int): Int = {
        value match {
          case Nil => acc
          case h::t => h match {
            case Some(x) => calcHeuristicCostRek(t, acc + calcOffset(index, x - 1), index + 1)
            case None => calcHeuristicCostRek(t, acc, index + 1)
          }
        }
    }
    calcHeuristicCostRek(value, 0, 0)
  }

  def calcOffset(actualIndex: Int, shouldIndex: Int): Int = {
    val actualIndexCoords = calcIndexIntoCoords(actualIndex)
    val shouldIndexCoords = calcIndexIntoCoords(shouldIndex)
    math.abs(actualIndexCoords._1 - shouldIndexCoords._1) + math.abs(actualIndexCoords._2 - shouldIndexCoords._2)
  }

  def calcIndexIntoCoords(index: Int): (Int, Int) = {
      (index / 4, index % 4)
  }

  /*val start = List( nix gut evtl
    Some(15), Some(14), Some(8), Some(12),
    Some(10), Some(11), Some(9), Some(13),
    Some(2), Some(6), Some(5), Some(1),
    Some(3), Some(7), Some(4), None
  )*/

  val start = List(
    Some(15), Some(8), Some(9), Some(14),
    Some(5), Some(10), Some(1), None,
    Some(12), Some(4), Some(3), Some(13),
    Some(11), Some(2), Some(7), Some(6))

 /* val start = List(
    Some(15), Some(8), Some(9), None,
    Some(5), Some(10), Some(1), Some(14),
    Some(12), Some(4), Some(3), Some(13),
    Some(11), Some(2), Some(7), Some(6)
  ) */

  val benniStart1 = List(
   Some(2), Some(3), Some(4), Some(8),
   Some(1), Some(6), Some(7), Some(12),
   Some(5), Some(10), Some(11), Some(15),
   Some(9), Some(13), Some(14), None
 )

  val benniStart2 = List(
    Some(3), Some(4), Some(8), Some(12),
    Some(2), Some(6), Some(7), Some(15),
    Some(1), Some(10), Some(11), None,
    Some(5), Some(9), Some(13), Some(14)
  )

  val vid = List(
    Some(7), Some(10), Some(1), Some(14),
    Some(6), Some(2), Some(9), Some(4),
    Some(3), Some(11), None, Some(5),
    Some(8), Some(12), Some(13), Some(15)
  )

  val easy = List(
    Some(1), Some(2), Some(3), Some(4),
    Some(5), Some(6), Some(7), Some(8),
    Some(9), Some(10), Some(11), Some(12),
    Some(13), None, Some(14), Some(15)
  )

  val t1 = System.nanoTime
  val resultNode = solvePuzzle(Node(benniStart2, List(), 0))
  val t2 = System.nanoTime
  resultNode.printPath()
  println("Dauer: " + (t2 - t1) / 1e9d + "s")
}