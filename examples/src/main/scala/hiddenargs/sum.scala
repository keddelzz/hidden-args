package hiddenargs

class sum {

  @hiddenargs
  def sum(xs: List[Int], @hidden acc: Int = 0): Int =
    xs match {
      case Nil      => acc
      case hd :: tl => sum(tl, acc + hd)
    }

  assert(sum(List.empty[Int])  == 0)
  assert(sum(List(0))          == 0)
  assert(sum(List(0, 1))       == 1)
  assert(sum(List(1, 2, 3))    == 6)
  assert(sum((1 to 10).toList) == 55)

}
