package Easy

object Solutions {

  /**
   * Challenge: https://www.hackerrank.com/challenges/functional-programming-warmups-in-recursion---gcd/problem
   */
  @scala.annotation.tailrec
  def gcd(x: Int, y: Int): Int = if (x == 0 || y == 0) x + y else gcd(y, x % y)

  /**
   * Challenge: https://www.hackerrank.com/challenges/functional-programming-warmups-in-recursion---fibonacci-numbers/problem
   */
  def fibonacci(n: Int): Int = {
    @scala.annotation.tailrec
    def tailRecursion(n: Int, a: Int, b: Int): Int = {
      if (n == 1) a
      else tailRecursion(n - 1, b, a + b)
    }

    tailRecursion(n, 0, 1)
  }

  /**
   * Challenge: https://www.hackerrank.com/challenges/pascals-triangle/problem
   */
  def drawPascalTriangle(numberOfLines: Int): Unit = {
    def factorial(n: Int): Int = if (n == 0) 1 else n * factorial(n - 1)

    (0 until numberOfLines).foreach(rowIndex => {
      val rowNumbers = (0 to rowIndex).map(columnIndex => {
        factorial(rowIndex) / (factorial(columnIndex) * factorial(rowIndex - columnIndex))
      }).toList
      println(rowNumbers.mkString(" "))
    })
  }

  /**
   * Challenge: https://www.hackerrank.com/challenges/string-mingling/problem
   */
  def getMingledString(x: String, y: String): String = {
    x.indices
      .map(index => List(x(index), y(index)))
      .toList
      .flatten
      .mkString
  }

  /**
   * Challenge: https://www.hackerrank.com/challenges/string-o-permute/problem
   * Swaps chars at even positions
   */
  def getSwappedString(str: String): String = {
    (0 until str.length by 2)
      .map(index => List(str(index + 1), str(index)))
      .toList
      .flatten
      .mkString
  }

  /**
   * Challenge: https://www.hackerrank.com/challenges/string-compression/problem
   * Replaces n occurrences of a character (call x) with {x}{n} if n > 1.
   * Example: aaabbc -> a3b2c
   */
  def getCompressedString(str: String): String = {
    @scala.annotation.tailrec
    def getTuples(tuples: List[(Char, Int)], str: String): List[(Char, Int)] = {
      if (str.isEmpty) {
        return tuples.reverse
      }
      val newString = str.drop(1)
      if (tuples.nonEmpty && tuples.head._1 == str.head) {
        val newHead = (tuples.head._1, tuples.head._2 + 1)
        val newTuplesList = newHead +: tuples.drop(1) // Replace the old head with new one
        getTuples(newTuplesList, newString)
      } else { // New character encountered, add a new tuple to head
        val newTuplesList = (str.head, 1) +: tuples
        getTuples(newTuplesList, newString)
      }
    }

    val tuples = getTuples(List(), str)
    tuples.map {
      case (chr, 1) => chr.toString
      case (chr, count) => s"$chr$count"
    }.mkString
  }

  /**
   * Challenge: https://www.hackerrank.com/challenges/prefix-compression/problem
   */
  def printCommonPrefix(x: String, y: String): Unit = {
    @scala.annotation.tailrec
    def getCommonPrefix(x: String, y: String, prefix: String = ""): String = {
      if (x.isEmpty || y.isEmpty || x.head != y.head)
        return prefix.reverse
      val newPrefix = x.head + prefix
      getCommonPrefix(x.drop(1), y.drop(1), newPrefix)
    }

    val commonPrefix = getCommonPrefix(x, y)

    def sliceAndPrint(str: String): Unit = {
      val sliced = str.drop(commonPrefix.length)
      println(s"${sliced.length} $sliced")
    }

    println(s"${commonPrefix.length} $commonPrefix")
    sliceAndPrint(x)
    sliceAndPrint(y)
  }

  /**
   * Challenge: https://www.hackerrank.com/challenges/string-reductions/problem
   */
  def removeDuplicateCharacters(str: String): String = {
    var seenChars: Set[Char] = Set()
    str.map(chr => {
      if (!seenChars.contains(chr)) {
        seenChars += chr
        Some(chr)
      } else None
    }).filter(charOption => charOption.isDefined)
      .map(charOption => charOption.get)
      .toList
      .mkString
  }

  /**
   * Challenge: https://www.hackerrank.com/challenges/functional-programming-the-sums-of-powers/problem
   * Given a value and power, finds the # of possible ways to obtain the value with summing up power of some
   * unique natural numbers.
   *
   * Note: Since problem's test cases are simple, DP is not needed.
   */
  def sumOfPowersCount(number: Int, power: Int): Int = {
    val upperLimit = Math.pow(number, 1.0 / power)

    def solve(current: Int = 1, sumSoFar: Int = 0, count: Int = 0): Int = {
      if (sumSoFar == number) {
        count + 1
      } else if (current <= upperLimit && sumSoFar < number) {
        val usedCase = solve(current + 1, math.pow(current, power).toInt + sumSoFar, count)
        val unUsedCase = solve(current + 1, sumSoFar, count)
        usedCase + unUsedCase
      } else count
    }

    solve()
  }

  /**
   * Challenge: https://www.hackerrank.com/challenges/sequence-full-of-colors/problem
   */
  def isFullOfSequenceColors(colorSequence: String): Boolean = {

    case class ColorCounts(R: Int = 0, G: Int = 0, Y: Int = 0, B: Int = 0) {
      def getIncreased(color: Char): ColorCounts = {
        color match {
          case 'R' => ColorCounts(R + 1, G, Y, B)
          case 'G' => ColorCounts(R, G + 1, Y, B)
          case 'Y' => ColorCounts(R, G, Y + 1, B)
          case 'B' => ColorCounts(R, G, Y, B + 1)
        }
      }

      def isWholeValid: Boolean = R == G && Y == B

      def isPrefixValid: Boolean = math.abs(R - G) <= 1 && math.abs(Y - B) <= 1
    }

    @scala.annotation.tailrec
    def solve(colorCounts: ColorCounts = ColorCounts(), index: Int = 0): Boolean = {
      if (index == colorSequence.length)
        colorCounts.isWholeValid
      else if (colorCounts.isPrefixValid) {
        solve(colorCounts.getIncreased(colorSequence.charAt(index)), index + 1)
      } else false
    }

    solve()
  }

  /**
   * Challenge: https://www.hackerrank.com/challenges/filter-elements/problem
   */
  def filterElementsByOccurrence(elements: List[Int], occurrenceLimit: Int): List[Int] = {
    import scala.collection.mutable
    // Use LinkedHashMap so that the elements would be generated by the iterator in the same order they are put.
    val occurrences: mutable.LinkedHashMap[Int, Int] = mutable.LinkedHashMap()
    elements.foreach(element => occurrences.put(element, occurrences.getOrElse(element, 0) + 1))
    occurrences.iterator
      .filter(_._2 >= occurrenceLimit)
      .map(_._1)
      .toList
  }

  /**
   * Challenge: https://www.hackerrank.com/challenges/lists-and-gcd/problem
   *
   * @return a string in form {x_1}{y_1}{x_2}{y_2}... where x is the factor and y is its power. (See challenge text
   *         for more details.)
   */
  def gcd(elements: List[List[Int]]): String = {
    // Convert list of int to Map of (factor, power) pairs as stated in the challenge.
    def toFactorPowerPair(factors: List[Int]): Map[Int, Int] = {
      factors.grouped(2)
        .map(factorPowerPair => factorPowerPair.head -> factorPowerPair(1))
        .toMap
    }

    val factorizationMaps = elements.map(toFactorPowerPair)

    def replaceWithMin(pair: (Int, Int)): (Int, Int) = {
      val factor = pair._1
      val min = factorizationMaps.map(factorizationMap => factorizationMap.getOrElse(factor, 0)).min
      factor -> min
    }

    factorizationMaps.head // Use any factorization here, as all of them will be compared with each other eventually.
      .map(replaceWithMin)
      .filter(pair => pair._2 > 0)
      .toSeq
      .sortBy(_._1)
      .map(a => s"${a._1} ${a._2}").mkString(" ")

  }

  /**
   * Challenge:  https://www.hackerrank.com/challenges/pentagonal-numbers/problem
   */
  def findPentagonalNumber(n: Long): Long = (0.5 * n * (3 * n - 1)).toLong

  /**
   * Challenge: https://www.hackerrank.com/challenges/fibonacci-fp/problem
   */
  def fibonacciWithModulo(cases: List[BigInt]): List[BigInt] = {
    val modulo: BigInt = 100000007

    // For memoization, calculate all fibonacci values between 0 and max case.
    @scala.annotation.tailrec
    def getAllFibonacciValues(n: BigInt, a: BigInt, b: BigInt, result: List[BigInt] = List()): List[BigInt] = {
      if (n == 0) (a :: result).reverse
      else getAllFibonacciValues(n - 1, b, ((a % modulo) + (b % modulo)) % modulo, a :: result)
    }

    getAllFibonacciValues(cases.max, 0, 1)
  }

  /**
   * Challenge: https://www.hackerrank.com/challenges/rotate-string/problem
   */
  def getAllRotatedStrings(str: String): List[String] = {
    @scala.annotation.tailrec
    def findRotatedStrings(n: Int, result: List[String] = List()): List[String] = {
      if (n == 0) result
      else if (result.isEmpty) findRotatedStrings(n - 1, List(str.drop(1) + str.charAt(0)))
      else findRotatedStrings(n - 1, (result.head.drop(1) + result.head.charAt(0)) :: result)
    }

    findRotatedStrings(str.length).reverse
  }

  /**
   * Challenge: https://www.hackerrank.com/challenges/missing-numbers-fp/problem
   */
  def findMissingNumbers(list1: List[Int], list2: List[Int]): List[Int] = {
    def findOccurrences(list: List[Int]): Map[Int, Int] = list
      .groupBy(i => i)
      .map(pair => pair._1 -> pair._2.size)

    val occurrences1 = findOccurrences(list1)
    val occurrences2 = findOccurrences(list2)
    occurrences1
      .filterNot(pair => pair._2 == occurrences2.getOrElse(pair._1, 0))
      .keys
      .toList
      .sortBy(i => i)
  }

  /**
   * Challenge: https://www.hackerrank.com/challenges/different-ways-fp/problem
   */
  def findDifferentWays(lemurCount: Int, teamSize: Int): BigInt = {
    val factorialMemory: Array[BigInt] = Array.fill(lemurCount + 1) {
      1
    } // Each index corresponds to related factorial value (with modulo)
    val modulo = 100000007

    // Fill the array for Memoization
    (1 to lemurCount).foreach(index => factorialMemory(index) = factorialMemory(index - 1) * index)

    // Calculate Combination(<lemurCount>, <teamSize>)
    val nominator = factorialMemory(lemurCount)
    val denominator = factorialMemory(teamSize) * factorialMemory(lemurCount - teamSize)
    (nominator / denominator) % modulo

  }

  /**
   * Challenge: https://www.hackerrank.com/challenges/huge-gcd-fp/problem
   */
  def findHugeGCD(firstNumber: List[BigInt], secondNumber: List[BigInt]): BigInt = {
    val modulo = 1000000007

    @scala.annotation.tailrec
    def gcd(x: BigInt, y: BigInt): BigInt = if (x == 0 || y == 0) x + y else gcd(y, x % y)

    gcd(firstNumber.product, secondNumber.product) % modulo
  }

  /**
   * Challenge: https://www.hackerrank.com/challenges/common-divisors/problem
   * All multipliers of a number is found by (factors) + (n / factor for each one)
   */
  def findCommonDivisorCount(x: Int, y: Int): Int = {
    def getDivisors(n: Int): Set[Int] = {
      val factors = (1 to math.sqrt(n).ceil.toInt).filter(n % _ == 0).toSet
      factors ++ factors.map(n / _)
    }

    getDivisors(x).intersect(getDivisors(y)).size
  }

  /**
   * Challenge: https://www.hackerrank.com/challenges/subset-sum/problem
   */
  def findMinSubsetSumSize(numbers: List[BigInt], cases: List[(Int, BigInt)]): List[BigInt] = {
    import scala.collection.mutable
    val sortedNumbers = numbers.sortWith((a, b) => a >= b)
    val sortedCases = cases.sortWith((a, b) => a._2 < b._2)

    val casesAndResultsPairs: mutable.Map[Int, BigInt] = mutable.Map()
    sortedCases.foreach(sortedCase => casesAndResultsPairs.put(sortedCase._1, -1))

    def mapToPrintableResultList(): List[BigInt] = {
      casesAndResultsPairs
        .map(a => (a._1, a._2))
        .toList
        .sortWith((a, b) => a._1 < b._1).map(_._2)
    }

    @scala.annotation.tailrec
    def recursivelySolve(sortedNumbers: List[BigInt], sortedCases: List[(Int, BigInt)], sumSoFar: BigInt = 0, count: Int = 1)
    : List[BigInt] = {
      (sortedNumbers, sortedCases) match {
        case (_, Nil) | (Nil, _) => mapToPrintableResultList()
        case (currentNumber :: numbersTail, currentCase :: casesTail) =>
          if (currentCase._2 <= sumSoFar + currentNumber) {
            casesAndResultsPairs.put(currentCase._1, count)
            recursivelySolve(sortedNumbers, casesTail, sumSoFar, count)
          } else {
            recursivelySolve(numbersTail, sortedCases, sumSoFar + currentNumber, count + 1)
          }
      }
    }

    recursivelySolve(sortedNumbers, sortedCases)
  }

}
