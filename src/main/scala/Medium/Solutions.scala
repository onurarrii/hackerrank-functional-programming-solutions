package Medium

object Solutions {
  /**
   * Challenge: https://www.hackerrank.com/challenges/super-digit/problem
   */
  def findSuperDigit(number: String, multiplier: Int): Int = {

    def findDigitSum(number: String, multiplier: Int = 1): String = (BigInt(number.map(_.asDigit).sum) * multiplier)
      .toString

    @scala.annotation.tailrec
    def helper(number: String): Int = {
      if (number.length == 1) number.head.asDigit
      else helper(findDigitSum(number))
    }

    helper(findDigitSum(number, multiplier))
  }

  /**
   * Challenge: https://www.hackerrank.com/challenges/captain-prime/problem
   */
  def findTravellerPosition(id: Int): String = {
    // Taken from: https://stackoverflow.com/a/36882293/10054375
    def isPrime(i: Int): Boolean = {
      if (i <= 1) false
      else if (i == 2) true
      else !(2 until i).exists(x => i % x == 0)
    }

    /**
     * @param isReverse if the parameter is true, then this means that number list is given in reverse, and computation
     *                  is going to be done accordingly.
     * @return whether all the subsequence of the number (direction is determined by <isReverse>) is prime
     */
    @scala.annotation.tailrec
    def areAllSequencesPrime(number: List[Int], isReverse: Boolean): Boolean = {
      number match {
        case Nil => true
        case 0 :: _ => false // The id should not contain 0 as a digit.
        case _ :: t =>
          val _number = if (isReverse) number.reverse else number
          isPrime(_number.mkString.toInt) && areAllSequencesPrime(t, isReverse)
      }
    }

    val idAsInts = id.toString.toList.map(_.asDigit)
    val isLeft = areAllSequencesPrime(idAsInts, isReverse = false)
    val isRight = areAllSequencesPrime(idAsInts.reverse, isReverse = true)

    if (isLeft && isRight) "CENTRAL"
    else if (isLeft) "LEFT"
    else if (isRight) "RIGHT"
    else "DEAD"

  }

  /**
   * Challenge: https://www.hackerrank.com/challenges/jumping-bunnies/problem
   */
  def lcm(list: List[BigInt]): BigInt = {
    @scala.annotation.tailrec
    def gcd(x: BigInt, y: BigInt): BigInt = if (x == 0 || y == 0) x + y else gcd(y, x % y)

    def lcm(x: BigInt, y: BigInt): BigInt = x * (y / gcd(x, y))

    @scala.annotation.tailrec
    def lcmOfNumbers(numbers: List[BigInt], result: BigInt = 1): BigInt = {
      numbers match {
        case Nil => result
        case number :: tail => lcmOfNumbers(tail, lcm(number, result))
      }
    }

    lcmOfNumbers(list)
  }

  /**
   * Challenge: https://www.hackerrank.com/challenges/swap-nodes/problem
   * Each node is represented with its value (starting from 1). After each swap operation, the new BT is printed to
   * stdout with using inorder traversal.
   *
   * @param nodes The pairs contain left and right children of node relevant node. Each index's element corresponds
   *              to node whose value is index + 1's (as counting starts from 1)
   * @param swaps Each swap value indicates a depth value. In the BT, if the node's depth value is a multiplier of
   *              the swap value, then the node's left and right substrees are swapped.
   */
  def printSwappedBTs(nodes: List[(Int, Int)], swaps: List[Int]): Unit = {
    import scala.collection.mutable

    case class Node(left: Int, right: Int)
    lazy val defaultNode = Node(-1, -1)

    // Stores id, node pairs
    val tree: mutable.Map[Int, Node] = nodes.zipWithIndex.map(p => {
      val pair = p._1
      val index = p._2
      index + 1 -> Node(pair._1, pair._2)
    }).to(collection.mutable.Map)

    /**
     * @return A list containing all nodes of <tree> whose order is determined accordingly to inorder traversal.
     */
    def inorderTraversal(result: List[Int] = List(), currentNode: Int = 1): List[Int] = {
      if (currentNode == -1) List()
      else {
        val node = tree.getOrElse(currentNode, defaultNode)
        (inorderTraversal(result, node.left) :+ currentNode) ++ inorderTraversal(result, node.right)
      }
    }

    def swapNodes(depth: Int): Unit = {

      /**
       * @return A list of nodes whose depth is a multiply of <depth> (such as 2 * depth, 3 * depth ... etc.)
       */
      def findNodesAtDepth(currentNode: Int = 1, currentDepth: Int = 1, result: List[Int] = List()): List[Int] = {
        if (currentNode == -1) List()
        else {
          val node = tree.getOrElse(currentNode, defaultNode)
          val previousResults = findNodesAtDepth(node.left, currentDepth + 1, result) ++
            findNodesAtDepth(node.right, currentDepth + 1, result)
          if (currentDepth % depth == 0) currentNode +: previousResults else previousResults
        }
      }

      findNodesAtDepth().foreach(nodeNumber => {
        val node = tree.getOrElse(nodeNumber, defaultNode)
        tree.put(nodeNumber, Node(node.right, node.left))
      })
    }

    swaps.map(swap => {
      swapNodes(swap)
      inorderTraversal().mkString(" ")
    }).foreach(println)

  }

  /**
   * Challenge: https://www.hackerrank.com/challenges/valid-bst/problem
   * The solution is adapted from: https://www.hackerrank.com/challenges/valid-bst/forum/comments/607832
   */
  def isValidBST(traversal: List[Int]): Boolean = {

    def helper(numbers: List[Int], lowerBound: Int, upperBound: Int): List[Int] = {
      numbers match {
        case Nil => List()
        case head :: tail =>
          if (head <= lowerBound || head >= upperBound) numbers
          else helper(helper(tail, lowerBound, head), head, upperBound)
      }
    }

    helper(traversal, Int.MinValue, Int.MaxValue).isEmpty
  }

  /**
   * Challenge: https://www.hackerrank.com/challenges/password-cracker-fp/problem
   */
  def crackPassword(passwords: Set[String], password: String): List[String] = {

    /**
     * If a password can be obtained by repeating another, then it means that existence of longer password do not
     * change the result. Thus, we can remove it to reduce complexity.
     */
    def narrowPasswordsSet(passwords: List[String], result: List[String] = List()): Set[String] = {
      passwords match {
        case Nil => result.toSet
        case x :: xs =>
          for (other <- passwords) {
            if (other != x && other * (x.length / other.length) == x) { // We can obtain the password by other
              return narrowPasswordsSet(xs, result)
            }
          }
          // There are no duplicate passwords. Put it in the set.
          narrowPasswordsSet(xs, x +: result)
      }
    }

    val narrowedPasswords = narrowPasswordsSet(passwords.toList)

    def solve(password: List[Char], remaining: String = "", result: List[String] = List()): List[String] = {
      password match {
        case x :: Nil =>
          val lastPassword = remaining + x
          if (narrowedPasswords.contains(lastPassword)) lastPassword +: result
          else List() // The list is exhausted, yet the <remaining> part is not matched with any password in the set.
        case x :: xs =>
          val foundPassword = remaining + x
          // Lazy val used here because if result is found by <usedCase> then we do not need to compute it.
          lazy val unusedCase = solve(xs, foundPassword, result)
          if (narrowedPasswords.contains(foundPassword)) { // If found password is not in the list, do not try usedCase.
            val usedCase = solve(xs, "", foundPassword +: result)
            if (usedCase.nonEmpty) usedCase else unusedCase
          } else unusedCase
      }
    }

    solve(password.toList)
  }

  /**
   * Challenge: https://www.hackerrank.com/challenges/number-of-binary-search-tree/problem
   * Solution is adapted from: https://www.geeksforgeeks.org/enumeration-of-binary-trees/
   */
  def findPossibleBSTCount(cases: List[Int]): List[BigInt] = {
    val modulo = 100000007
    // Use max case to fill up data for memoization.
    val memory = new Array[BigInt](cases.max + 1)
    memory(0) = 1
    memory(1) = 1
    (2 until memory.length).foreach(index => {
      memory(index) = (0 until index).map(i => memory(i) * memory(index - i - 1) % modulo).sum % modulo
    })
    cases.map(memory(_))
  }


}
