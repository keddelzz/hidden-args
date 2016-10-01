package hiddenargs

class fibonacci {

  /*
   * tail-recursive implementation of fibonacci function
   * taken from: http://rosettacode.org/wiki/Fibonacci_sequence#Scala
   */
  @hiddenargs
  def fibonacci(n: Int, @hidden prev: BigInt = 0, @hidden next: BigInt = 1): BigInt = n match {
    case 0 => prev
    case 1 => next
    case _ => fibonacci(n - 1, next, next + prev)
  }

  assert(fibonacci(0)  == BigInt(0))
  assert(fibonacci(1)  == BigInt(1))
  assert(fibonacci(2)  == BigInt(1))
  assert(fibonacci(3)  == BigInt(2))
  assert(fibonacci(4)  == BigInt(3))
  assert(fibonacci(5)  == BigInt(5))
  assert(fibonacci(6)  == BigInt(8))
  assert(fibonacci(12) == BigInt(144))
  assert(fibonacci(50) == BigInt("12586269025"))

}
