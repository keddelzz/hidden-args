package hiddenargs

import org.scalatest._
import shapeless.test.illTyped

class HiddenArgsSpec extends FlatSpec with Matchers {

  "Hidden Arguments" should "be removed during macro expansion" in {
    @hiddenargs
    def factorial(n: Int, @hidden acc: BigInt = BigInt(1)): BigInt =
      if (n <= 0) acc
      else factorial(n - 1, acc * BigInt(n))

    factorial(0)  should be (BigInt(1))
    factorial(1)  should be (BigInt(1))
    factorial(2)  should be (BigInt(2))
    factorial(3)  should be (BigInt(6))
    factorial(4)  should be (BigInt(24))
    factorial(5)  should be (BigInt(120))
    factorial(6)  should be (BigInt(720))
    factorial(18) should be (BigInt("6402373705728000"))

    illTyped("factorial(3, 1)")
  }

  "Multiple hidden arguments" should "be supported" in {
    @hiddenargs
    def foo(n: Int, @hidden acc1: Int = 0, @hidden acc2: Int = 0): (Int, Int) =
      if (n <= 0) (acc1, acc2)
      else foo(n - 1, acc1 + 1, acc2 - 1)

    foo(3) should be (3, -3)
    illTyped("foo(5, 1)")
    illTyped("foo(5, 1, 2)")
  }

  "Hidden arguments" should "need a default value" in {
    illTyped("""
      @hiddenargs
      def factorial(n: Int, @hidden acc: BigInt): BigInt =
        if (n <= 0) acc
        else factorial(n - 1, acc * BigInt(n))
    """, "Hidden function parameter 'acc' needs a default value!")
  }

}
