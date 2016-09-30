package hiddenargs

class factorial {

  @hiddenargs
  def factorial(n: Int, @hidden acc: BigInt = BigInt(1)): BigInt =
    if (n <= 0) acc
    else factorial(n - 1, acc * BigInt(n))

  assert(factorial(0)  == BigInt(1))
  assert(factorial(1)  == BigInt(1))
  assert(factorial(2)  == BigInt(2))
  assert(factorial(3)  == BigInt(6))
  assert(factorial(4)  == BigInt(24))
  assert(factorial(5)  == BigInt(120))
  assert(factorial(6)  == BigInt(720))
  assert(factorial(18) == BigInt("6402373705728000"))

}
