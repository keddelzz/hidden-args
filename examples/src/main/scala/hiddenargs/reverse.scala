package hiddenargs

class reverse {

  @hiddenargs
  def reverse[T](xs: List[T], @hidden acc: List[T] = Nil): List[T] =
    xs match {
      case Nil      => acc
      case hd :: tl => reverse(tl, hd :: acc)
    }

  assert(reverse(List(1, 2, 3))    == List(3, 2, 1))
  assert(reverse("Hello!".toList)  == "!olleH".toList)
  assert(reverse(List('a, 'b, 'c)) == List('c, 'b, 'a))
  assert(reverse(Nil)              == Nil)

}
