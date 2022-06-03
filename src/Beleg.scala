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

    def printPath(): Unit

    def printTable(): Unit

    def search(): Unit
  }

  case object Leaf extends Tree {
    def isCorrect: Boolean = false

    def search(): Unit = None

    def printPath(): Unit = None

    def printTable(): Unit = None
  }

  case class Node(down: Tree, up: Tree, left: Tree, right: Tree, value: List[Option[Int]], history: List[Char]) extends Tree {
    def isCorrect: Boolean = value.equals(sortedList)

    def search(): Unit = {
      if (isCorrect) println(history)
      else {
        down.search()
        up.search()
        left.search()
        right.search()
      }
    }

    // TODO: Tiefe implementierten

    def printPath(): Unit = {
      println(history)
      down.printPath()
      up.printPath()
      left.printPath()
      right.printPath()
    }

    def printTable(): Unit = {
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

  def moveLeft(value: List[Option[Int]], depth: Int, history: List[Char]): Tree = {
    val emptyIndex = value.indexOf(None)
    if (emptyIndex % 4 == 0 || depth == 0) Leaf
    else {
      val newVal = value.slice(0, emptyIndex - 1) ::: None :: value(emptyIndex - 1) :: value.slice(emptyIndex + 1, 16)
      val newDepth = depth - 1
      val newHistory = history :+ 'l'
      Node(moveDown(newVal, newDepth, newHistory), moveUp(newVal, newDepth, newHistory),
        moveLeft(newVal, newDepth, newHistory), Leaf, newVal, newHistory)
    }
  }

  def moveRight(value: List[Option[Int]], depth: Int, history: List[Char]): Tree = {
    val emptyIndex = value.indexOf(None)
    if (emptyIndex == 3 || emptyIndex == 7 || emptyIndex == 11 || emptyIndex == 15 || depth == 0) Leaf
    else {
      val newVal = value.slice(0, emptyIndex) ::: value(emptyIndex + 1) :: None :: value.slice(emptyIndex + 2, 16)
      val newDepth = depth - 1
      val newHistory = history :+ 'r'
      Node(moveDown(newVal, newDepth, newHistory), moveUp(newVal, newDepth, newHistory),
        Leaf, moveRight(newVal, newDepth, newHistory), newVal, newHistory)
    }
  }

  def moveUp(value: List[Option[Int]], depth: Int, history: List[Char]): Tree = {
    val emptyIndex = value.indexOf(None)
    if (emptyIndex <= 3 || depth == 0) Leaf
    else {
      val newVal = value.slice(0, emptyIndex - 4) ::: None :: value.slice(emptyIndex - 3, emptyIndex) ::: value(emptyIndex - 4) :: value.slice(emptyIndex + 1, 16)
      val newDepth = depth - 1
      val newHistory = history :+ 'u'
      Node(Leaf, moveUp(newVal, newDepth, newHistory), moveLeft(newVal, newDepth, newHistory),
        moveRight(newVal, newDepth, newHistory), newVal, newHistory)
    }
  }

  def moveDown(value: List[Option[Int]], depth: Int, history: List[Char]): Tree = {
    val emptyIndex = value.indexOf(None)
    if (emptyIndex >= 12 || depth == 0) Leaf
    else {
      val newVal = value.slice(0, emptyIndex) ::: value(emptyIndex + 4) :: value.slice(emptyIndex + 1, emptyIndex + 4) ::: None :: value.slice(emptyIndex + 5, 16)
      val newDepth = depth - 1
      val newHistory = history :+ 'd'
      Node(moveDown(newVal, newDepth, newHistory), Leaf,
        moveLeft(newVal, newDepth, newHistory), moveRight(newVal, newDepth, newHistory), newVal, newHistory)
    }
  }

  def createTree(value: List[Option[Int]], depth: Int): Tree = {
    Node(moveDown(value, depth, List()), moveUp(value, depth, List()),
      moveLeft(value, depth, List()), moveRight(value, depth, List()), value, List())
  }

  def calcHeuristic(value: List[Option[Int]]): Int = {
    @tailrec
    def calcHeuristicRek(value: List[Option[Int]], acc: Int, index: Int): Int = {
        value match {
          case Nil => acc
          case h::t => h match {
            case Some(x) => calcHeuristicRek(t, calcOffset(index, x - 1), index + 1)
            case None => calcHeuristicRek(t, calcOffset(index, 15), index + 1)
          }
        }
    }

    calcHeuristicRek(value, 0, 0)
  }

  def calcOffset(actualIndex: Int, shouldIndex: Int): Int = {
    if (actualIndex > 15 || shouldIndex > 15) println("FEHLER") // TODO DEBUG
    val actualIndexCoords = calcIndexIntoCoords(actualIndex)
    val shouldIndexCoords = calcIndexIntoCoords(shouldIndex)
    math.abs(actualIndexCoords._1 - shouldIndexCoords._1) + math.abs(actualIndexCoords._2 - shouldIndexCoords._2)
  }

  def calcIndexIntoCoords(index: Int): (Int, Int) = {
      (index / 4, index % 4)
  }

  /* def searchTree(start: List[Option[Int]]): Node = {
    def searchTreeRek(start: List[Option[Int]], depth: Int): Node = {
      val tree = createTree(start, depth)

    }
  }
}*/

  val start = List(
    Some(1), Some(2), Some(3), Some(4),
    Some(5), Some(6), Some(7), Some(8),
    Some(9), Some(10), None, Some(12),
    Some(13), Some(14), Some(11), Some(15))

  //val tree = createTree(start, 12)
  //tree.search()

  println(calcOffset(15, 3))
}


// f = g+h
// mit h: Länge des Pfaddes vom Start bis zum aktuellen Knoten
// h: Heuristische Schätzfunktion, darf Entfernung bis zum Ziel nicht überschätzen
// Dynamisch erzeigen, also interativ einfach depth bei createTree mitgeben
// mit Manhatten Metrik
