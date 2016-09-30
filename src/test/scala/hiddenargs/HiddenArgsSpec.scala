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

  "Hidden arguments" should "not be implicit" in {
    illTyped("""
      @hiddenargs
      def foobar(implicit @hidden acc: BigInt = BigInt(0)): BigInt =
        acc
    """, "Hidden function parameter 'acc' can't be implicit!")
  }

  "Hidden arguments" should "support usage of implicit parameters" in {
    @hiddenargs
    def sum[T](xs: List[T], @hidden acc: T = ev.zero)(implicit ev: Numeric[T]): T =
      xs match {
        case Nil      => acc
        case hd :: tl => sum(tl, ev.plus(hd, acc))
      }

    sum(List[Byte](1, 2, 3))  should be (6.toByte)
    sum(List[Short](1, 2, 3)) should be (6.toShort)
    sum(List[Int](1, 2, 3))   should be (6)
    sum(List[Long](1, 2, 3))  should be (6L)

    @hiddenargs
    def sum2[T: Numeric](xs: List[T], @hidden acc: T = implicitly[Numeric[T]].zero): T =
      xs match {
        case Nil      => acc
        case hd :: tl => sum2(tl, implicitly[Numeric[T]].plus(hd, acc))
      }

    sum2(List[Byte](1, 2, 3))  should be (6.toByte)
    sum2(List[Short](1, 2, 3)) should be (6.toShort)
    sum2(List[Int](1, 2, 3))   should be (6)
    sum2(List[Long](1, 2, 3))  should be (6L)
  }

  "Annotations of functions with 'hiddenargs'" should "be moved to the nested function" in {
    /*
     * @tailrec
     * @hiddenargs
     * def factorial5(n: Int, @hidden acc: Int = 5): Int =
     *   if (n <= 0) 1
     *   else n * factorial5(n - 1, acc)
     *
     * I'm not sure how to test this, however the function above expands to
     *
     * def factorial5(n: Int): Int = {
     *   @new tailrec()
     *   def factorial5_impl(n: Int, acc: Int): Int =
     *     if (n.$less$eq(0))
     *       1
     *     else
     *       n.$times(factorial5_impl(n.$minus(1), acc));
     *
     *    factorial5_impl(n, 5)
     *  }
     *
     *  This causes a compile error, because `factorial5_impl` is not
     *  tail-recursive, this shows that the feature of moving annotations
     *  works correctly.
     */
  }

}
