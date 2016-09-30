package hiddenargs

class genericsum {

  @hiddenargs
  def gsum[T](xs: List[T], @hidden acc: T = ev.zero)(implicit ev: Numeric[T]): T =
    xs match {
      case Nil      => acc
      case hd :: tl => gsum(tl, ev.plus(hd, acc))
    }

  assert(gsum(List.empty[Byte]) == 0.toByte)
  assert(gsum(List[Short](0))   == 0.toShort)
  assert(gsum(List(0, 1))       == 1)
  assert(gsum(List(1L, 2L, 3L)) == 6L)

  val oneToTen = (1 to 10 map (BigInt(_))).toList
  assert(gsum(oneToTen)         == BigInt(55))

}
