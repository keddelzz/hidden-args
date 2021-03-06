package hiddenargs

import scala.annotation.compileTimeOnly
import scala.annotation.StaticAnnotation
import scala.language.experimental.macros

@compileTimeOnly("enable macro paradise to expand macro annotations")
class hiddenargs extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro HiddenMacros.transform
}
