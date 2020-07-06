package Introduction

object Solutions {

  /**
   * Challenge: https://www.hackerrank.com/challenges/fp-hello-world-n-times/problem
   */
  def printHelloWorldNTimes(n: Int): Unit = for (_ <- 1 to n) println("Hello World")

  /**
   * Challenge: https://www.hackerrank.com/challenges/fp-array-of-n-elements/problem
   */
  def getArrayOfNElements(num: Int): List[Int] = (1 to num).toList

  /**
   * Challenge: https://www.hackerrank.com/challenges/fp-filter-array/problem
   */
  def getElementsBelowDelim(delim: Int, arr: List[Int]): List[Int] = arr.filter(elem => elem < delim)

  /**
   * Challenge: https://www.hackerrank.com/challenges/fp-filter-positions-in-a-list/problem
   */
  def getOddIndexedElements(arr: List[Int]): List[Int] = for {
    (elem, index) <- arr.zipWithIndex
    if index % 2 == 1
  } yield elem

  /**
   * Challenge: https://www.hackerrank.com/challenges/fp-list-replication/problem
   */
  def tabulateElementsNTimes(times: Int, elements: List[Int]): List[Int] = {
    elements.flatMap(element => List.tabulate(times)(_ => element))
  }

  /**
   * Challenge: https://www.hackerrank.com/challenges/fp-reverse-a-list/problem
   */
  def reverse(arr: List[Int]): List[Int] = arr.reverse

  /**
   * Challenge: https://www.hackerrank.com/challenges/fp-sum-of-odd-elements/problem
   */
  def getSumOfOddElements(arr: List[Int]): Int = arr.filter(_ % 2 != 0).sum

  /**
   * Challenge: https://www.hackerrank.com/challenges/fp-list-length/problem
   */
  def getLength(arr: List[Int]): Int = arr.length

  /**
   * Challenge: https://www.hackerrank.com/challenges/fp-update-list/problem
   */
  def replaceElementsWithAbs(arr: List[Int]): List[Int] = arr.map(Math.abs)

  /**
   * Calculates power(e, number)
   * Challenge: https://www.hackerrank.com/challenges/eval-ex/problem
   */
  def evaluateExponential(number: Double): Double = {
    var nominator: Double = 1.0
    var denominator: Double = 1.0
    // Skip the first element as it will always be "1" and its calculation is not possible with the following algorithm.
    val firstElement = 1
    val seriesExpansion = (1 to 9).map(index => {
      nominator *= number
      denominator *= index.toDouble
      nominator / denominator
    }).sum
    firstElement + seriesExpansion // Add the omitted first element
  }

  /**
   * Challenge: https://www.hackerrank.com/challenges/functions-or-not/problem
   */
  def isFunction(pairs: List[(Int, Int)]): Boolean = {
    // for f(x) = y, x values should be unique. (Assumed each input line will be unique.)
    pairs.size == pairs.map(pair => pair._1).toSet.size
  }

  /**
   * Challenge: https://www.hackerrank.com/challenges/lambda-march-compute-the-perimeter-of-a-polygon/problem
   */
  def getPerimeter(points: List[(Double, Double)]): Double = {
    def findDistance(point1: (Double, Double), point2: (Double, Double)): Double = {
      math.sqrt(math.pow(point1._1 - point2._1, 2) + math.pow(point1._2 - point2._2, 2))
    }

    // Add first one to end to get circular points
    val circularPoints = points :+ points.head
    points.indices.map(index => findDistance(circularPoints(index), circularPoints(index + 1))).sum

  }

  /**
   * Challenge: https://www.hackerrank.com/challenges/lambda-march-compute-the-area-of-a-polygon/problem
   * Formula is taken from: https://www.mathopenref.com/coordpolygonarea.html
   */
  def getArea(points: List[(Double, Double)]): Double = {
    val circularPoints = points :+ points.head
    val sum = points.indices.map(index => {
      val point1 = circularPoints(index)
      val point2 = circularPoints(index + 1)
      point1._1 * point2._2 - point1._2 * point2._1
    }).sum
    math.abs(sum / 2)
  }

}
