package hiddenargs

import org.scalatest._

class ExampleSpec extends FlatSpec with Matchers {

  "The example 'factorial'" should "be valid" in {
    new factorial()
  }

  "The example 'sum'" should "be valid" in {
    new sum()
  }

  "The example 'genericsum'" should "be valid" in {
    new genericsum()
  }

  "The example 'count'" should "be valid" in {
    new count()
  }

  "The example 'fibonacci'" should "be valid" in {
    new fibonacci()
  }

}
