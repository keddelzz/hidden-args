package privateargs

import scala.reflect.macros.whitebox.Context
import scala.collection.mutable.ListBuffer
import java.io.Serializable

private[privateargs] class HiddenMacros(val c: Context) {
  import c.universe._

  private def replaceCall(body: Tree, orig: TermName, replacement: TermName): Tree = {
    body
  }

  private val hiddenTpe = typeOf[privateargs.hidden]

  private sealed trait Param extends Product with Serializable
  private case class Normal(param: Tree, name: TermName) extends Param
  private case class Hidden(mods: Modifiers, name: TermName, tpe: Tree, default: Tree) extends Param

  private def paramInfos(params: List[Tree]): (List[Param], Boolean) = {
    def isHiddenParameter(mods: Modifiers): Boolean =
      mods.annotations exists hiddenAnnotation

    def hiddenAnnotation(annot: Tree): Boolean = {
      val typedAnnot = c.typecheck(annot, silent = true)
      typedAnnot.nonEmpty && typedAnnot.tpe =:= hiddenTpe
    }

    val parameters = params map {
      case t @ q"$mods val $name: $tpe = $default" if isHiddenParameter(mods) =>
        if (default.isEmpty) {
          val paramName = name.decodedName.toString
          c.error(c.enclosingPosition, s"Hidden function parameter '$paramName' needs a default value!")
          Normal(t, name)
        } else {
          val newMods = mods mapAnnotations (_ filterNot hiddenAnnotation)
          Hidden(newMods, name, tpe, default)
        }
      case t @ q"$mods val $name: $tpe" =>
        Normal(t, name)
      case _ =>
        c.abort(c.enclosingPosition, "Unsupported shape of parameter!")
    }

    val hasHiddenParameters = parameters exists {
      case _: Hidden => true
      case _: Normal => false
    }

    if (!hasHiddenParameters) {
      c.warning(c.enclosingPosition, s"Annotation 'privateargs' was used but no parameter was marked as private using the annotation 'hidden'!")
    }

    (parameters, hasHiddenParameters)
  }

  def transform(annottees: Tree*): Tree = {
    val res = annottees map {
      case tree @ q"""
        def $funName[..$ptys](..$params): $retTy = $funBody
        """ =>
        val (parameters, shouldTransform) = paramInfos(params)

        if (shouldTransform) {
          val funImplName = TermName(funName.decodedName.toString() + "_impl")
          val newBody = replaceCall(funBody, funName, funImplName)

          /*
           * Drop default value and 'hidden' annotation
           * in the parameter-list of outer and inner
           * function.
           */
          val newParams = parameters map {
            // mods: Modifiers, name: TermName, tpe: Tree, default: Tree
            case Hidden(mods, name, tpe, _) =>
              q"$mods val $name: $tpe"
            case Normal(p, _) => p
          }

          /*
           * Parse default value of hidden parameters and parameters
           * to inner function.
           */
          val args = parameters map {
            case Hidden(_, _, _, default) => default
            case Normal(_, name)          => q"$name"
          }

          q"""
          def $funName[..$ptys](..$newParams): $retTy = {
            def $funImplName(..$newParams): $retTy = $newBody

            $funImplName(..$args)
          }
          """
        } else {
          tree
        }

      case t =>
        c.error(c.enclosingPosition, "Unsupported usage of annotation 'privateargs'!")
        t
    }

    q"{..$res}"
  }

}
