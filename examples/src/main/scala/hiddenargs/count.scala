package hiddenargs

class count {

  @hiddenargs
  def count[T](xs: List[T], @hidden acc: Int = 0): Int =
    xs match {
      case Nil     => acc
      case _ :: tl => count(tl, acc + 1)
    }

  assert(count(List(1, 2, 3))    == 3)
  assert(count(List.empty[Char]) == 0)

  val aToZ = 'a' to 'z'
  assert(count(aToZ.toList)      == 26)

}
