package hiddenargs

import org.scalatest._

class ExampleSpec extends FlatSpec with Matchers {

  private def example(name: String, run: => Unit): Unit =
    s"The example '$name'" should "be valid" in (run)

  example("count",      new count())
  example("factorial",  new factorial())
  example("fibonacci",  new fibonacci())
  example("genericsum", new genericsum())
  example("reverse",    new reverse())
  example("sum",        new sum())

}
