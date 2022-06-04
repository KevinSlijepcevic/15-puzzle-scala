import scala.annotation.tailrec

object Beleg extends App {
  // Beleg
  // 16 Verschiebe Puzzle programmieren also 1 2 3 4 / 5 6 7 8 / 9 10 11 12 / 13 14 15 16

  // IDA*-Suche
  // iterative Tiefensuche
  // nicht erlaubt: var und return
  // map verwenden

  // Abgabe: 20.06

  val sortedList = List(Some(1), Some(2), Some(3), Some(4),
    Some(5), Some(6), Some(7), Some(8),
    Some(9), Some(10), Some(11), Some(12),
    Some(13), Some(14), Some(15), None)

  trait Tree {
    def isCorrect: Boolean
    def printTable(): Unit
    def printSolution(): Unit
    def getMaxCost: Int
    def foundSolution: Boolean
  }

  case class Leaf(cost: Int) extends Tree {
    def isCorrect: Boolean = false
    def printSolution(): Unit = None
    def printTable(): Unit = None
    def getMaxCost: Int = cost
    def foundSolution: Boolean = false
  }

  case class Node(down: Tree, up: Tree, left: Tree, right: Tree, value: List[Option[Int]], history: List[Char], cost: Int) extends Tree {
    def isCorrect: Boolean = value.equals(sortedList)

    def printSolution(): Unit = {
      if (isCorrect) println("Path: " + history.toString() + " Moves: " + history.size)
      else {
        down.printSolution()
        up.printSolution()
        left.printSolution()
        right.printSolution()
      }
    }

    def foundSolution: Boolean = {
      isCorrect || down.foundSolution || up.foundSolution || left.foundSolution || right.foundSolution
    }

    def getMaxCost: Int = {
      if (cost < down.getMaxCost) down.getMaxCost
      else if (cost < up.getMaxCost) up.getMaxCost
      else if (cost < left.getMaxCost) left.getMaxCost
      else if (cost < right.getMaxCost) right.getMaxCost
      else cost
    }

    def printTable(): Unit = {
      println("History: " + history)
      println("Cost: " + cost)
      println(value.slice(0, 4))
      println(value.slice(4, 8))
      println(value.slice(8, 12))
      println(value.slice(12, 16))
      println()
      down.printTable()
      up.printTable()
      left.printTable()
      right.printTable()
    }
  }

  def moveLeft(value: List[Option[Int]], history: List[Char], bound: Int): Tree = {
    val emptyIndex = value.indexOf(None)
    if (emptyIndex % 4 == 0) Leaf(0)
    else {
      val newVal = value.slice(0, emptyIndex - 1) ::: None :: value(emptyIndex - 1) :: value.slice(emptyIndex + 1, 16)
      val currentCost = calcHeuristicCost(newVal) + history.size + 1
      if (currentCost > bound) Leaf(currentCost)
      else {
        val newHistory = history :+ 'l'
        Node(
          moveDown(newVal, newHistory, bound),
          moveUp(newVal, newHistory, bound),
          moveLeft(newVal, newHistory, bound),
          Leaf(0),
          newVal,
          newHistory,
          currentCost
        )
      }
    }
  }

  def moveRight(value: List[Option[Int]], history: List[Char], bound: Int): Tree = {
    val emptyIndex = value.indexOf(None)
    if (emptyIndex == 3 || emptyIndex == 7 || emptyIndex == 11 || emptyIndex == 15) Leaf(0)
    else {
      val newVal = value.slice(0, emptyIndex) ::: value(emptyIndex + 1) :: None :: value.slice(emptyIndex + 2, 16)
      val currentCost = calcHeuristicCost(newVal) + history.size + 1
      if (currentCost > bound) Leaf(currentCost)
      else {
        val newHistory = history :+ 'r'
        Node(
          moveDown(newVal, newHistory, bound),
          moveUp(newVal, newHistory, bound),
          Leaf(0),
          moveRight(newVal, newHistory, bound),
          newVal,
          newHistory,
          currentCost
        )
      }
    }
  }

  def moveUp(value: List[Option[Int]], history: List[Char], bound: Int): Tree = {
    val emptyIndex = value.indexOf(None)
    if (emptyIndex <= 3) Leaf(0)
    else {
      val newVal = value.slice(0, emptyIndex - 4) ::: None :: value.slice(emptyIndex - 3, emptyIndex) ::: value(emptyIndex - 4) :: value.slice(emptyIndex + 1, 16)
      val currentCost = calcHeuristicCost(newVal) + history.size + 1
      if (currentCost > bound) Leaf(currentCost)
      else {
        val newHistory = history :+ 'u'
        Node(
          Leaf(0),
          moveUp(newVal, newHistory, bound),
          moveLeft(newVal, newHistory, bound),
          moveRight(newVal, newHistory, bound),
          newVal,
          newHistory,
          currentCost
        )
      }
    }
  }

  def moveDown(value: List[Option[Int]], history: List[Char], bound: Int): Tree = {
    val emptyIndex = value.indexOf(None)
    if (emptyIndex >= 12) Leaf(0)
    else {
      val newVal = value.slice(0, emptyIndex) ::: value(emptyIndex + 4) :: value.slice(emptyIndex + 1, emptyIndex + 4) ::: None :: value.slice(emptyIndex + 5, 16)
      val currentCost = calcHeuristicCost(newVal) + history.size + 1
      if (currentCost > bound) Leaf(currentCost)
      else {
        val newHistory = history :+ 'd'
        Node(
          moveDown(newVal, newHistory, bound),
          Leaf(0),
          moveLeft(newVal, newHistory, bound),
          moveRight(newVal, newHistory, bound),
          newVal,
          newHistory,
          currentCost
        )
      }
    }
  }

  def createTree(value: List[Option[Int]], bound: Int): Tree = {
    Node(
      moveDown(value, List(), bound),
      moveUp(value, List(), bound),
      moveLeft(value, List(), bound),
      moveRight(value, List(), bound),
      value,
      List(),
      0
    )
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

  def startIDA(start: List[Option[Int]]): Unit = {
    @tailrec
    def startIDARec(start: List[Option[Int]], bound: Int): Unit = {
        println("NEW RUN WITH COST: " + bound) // DEBUG
        val tree = createTree(start, bound)
        if (tree.foundSolution) tree.printSolution()
        else startIDARec(start, tree.getMaxCost)
    }
    startIDARec(start, calcHeuristicCost(start))
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


  val puzzle1 = Array(
    2,  3,  4,  8,
    1,  6,  7, 12,
    5, 10, 11, 15,
    9, 13, 14,  0
  )

  val puzzle2 = Array(
    3,  4,  8, 12,
    2,  6,  7, 15,
    1, 10, 11, 14,
    5,  9, 13,  0
  )


  //val tree = createTree(start, 150)
  //tree.printSolution()
  //tree.printTable()
  //println(calcHeuristicCost(start))
  startIDA(start)

}


// f = g+h
// mit h: Länge des Pfaddes vom Start bis zum aktuellen Knoten
// h: Heuristische Schätzfunktion, darf Entfernung bis zum Ziel nicht überschätzen
// Dynamisch erzeigen, also interativ einfach depth bei createTree mitgeben
// mit Manhatten Metrik
