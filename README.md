[![Build Status](https://travis-ci.org/keddelzz/hidden-args.svg?branch=master)](https://travis-ci.org/keddelzz/hidden-args)
[![Build status](https://ci.appveyor.com/api/projects/status/97g1o2chcosdada2/branch/master?svg=true)](https://ci.appveyor.com/project/keddelzz/hidden-args/branch/master)

# Hidden Arguments

## Description

A common pattern during development of tail-recursive functions is to
introduce a helper function to hide the accumulator parameter.

**Hidden Arguments** is a project that provides a language feature
(implemented in Scala using macros), which helps the developer to
define such functions without the necessary boilerplate.

--

The definition of the function 

```scala
@hiddenargs
def sum(xs: List[Int], @hidden acc: Int = 0): Int = 
  xs match {
    case hd :: tl => sum(tl, hd + acc)
    case _        => acc
  }
```

will be rewritten to hide the parameter `acc`:

```scala
def sum(xs: List[Int]): Int = {
  def sum_impl(xs: List[Int], acc: Int): Int =
    xs match {
      case hd :: tl => sum_impl(tl, hd + acc)
      case _        => acc
    }

  sum_impl(xs, 0)
}
```

--

The defined function, which should be rewritten, has to be marked with
the annotation 'hiddenargs'. Every parameter of such function, which
is marked with the 'hidden' annotation, will be hidden using a nested
function. The default value of the hidden paramter is used to call the
nested function.

## License

See the [LICENSE](LICENSE) file for license rights and limitations (MIT).
