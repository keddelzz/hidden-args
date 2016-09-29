package hiddenargs

import scala.reflect.macros.whitebox.Context
import scala.collection.mutable.ListBuffer
import java.io.Serializable

private[hiddenargs] class HiddenMacros(val c: Context) {
  import c.universe._

  private def replaceCall(body: Tree, orig: TermName, replacement: TermName): Tree = {
    val transformer = new Transformer {
      override def transform(tree: Tree): Tree =
        super.transform(tree match {
          case Ident(x) if x == orig => Ident(replacement)
          case x                     => x
        })
    }
    transformer.transform(body)
  }

  private def transformMods(mods: Modifiers): Modifiers = {
    val flagValues = Seq(
      Flag.TRAIT,
      Flag.INTERFACE,
      Flag.MUTABLE,
      Flag.MACRO,
      Flag.DEFERRED,
      Flag.ABSTRACT,
      Flag.FINAL,
      Flag.SEALED,
      Flag.IMPLICIT,
      Flag.LAZY,
      Flag.OVERRIDE,
      Flag.PRIVATE,
      Flag.PROTECTED,
      Flag.LOCAL,
      Flag.CASE,
      Flag.ABSOVERRIDE,
      Flag.BYNAMEPARAM,
      Flag.PARAM,
      Flag.COVARIANT,
      Flag.CONTRAVARIANT,
      Flag.DEFAULTPARAM,
      Flag.PRESUPER,
      Flag.DEFAULTINIT,
      Flag.ENUM,
      Flag.PARAMACCESSOR,
      Flag.CASEACCESSOR,
      Flag.SYNTHETIC,
      Flag.ARTIFACT,
      Flag.STABLE)

    /*
     * Rebuild 'oldFlags', but don't add flag 'DEFAULTPARAM'
     */
    var flags = NoFlags
    for (flag <- flagValues) {
      if ((mods hasFlag flag) && flag != Flag.DEFAULTPARAM) {
        flags |= flag
      }
    }

    Modifiers(
      flags,
      mods.privateWithin,
      mods.annotations filterNot hiddenAnnotation)
  }

  private val hiddenTpe = typeOf[hiddenargs.hidden]

  private sealed trait Param extends Product with Serializable
  private case class Normal(param: Tree, name: TermName) extends Param
  private case class Hidden(mods: Modifiers, name: TermName, tpe: Tree, default: Tree) extends Param

  private def isHiddenParameter(mods: Modifiers): Boolean =
    mods.annotations exists hiddenAnnotation

  private def hiddenAnnotation(annot: Tree): Boolean = {
    val typedAnnot = c.typecheck(annot, silent = true)
    typedAnnot.nonEmpty && typedAnnot.tpe =:= hiddenTpe
  }

  private def paramInfos(params: List[Tree]): (List[Param], Boolean) = {
    val parameters = params map {
      case t @ q"$mods val $name: $tpe = $default" if isHiddenParameter(mods) =>
        if (default.isEmpty) {
          val paramName = name.decodedName.toString
          c.error(c.enclosingPosition, s"Hidden function parameter '$paramName' needs a default value!")
          Normal(t, name)
        } else {
          val newMods = transformMods(mods)
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
           * Drop default arguments, which were marked
           * with 'hidden'.
           */
          val outerParams = parameters collect {
            case Normal(p, _) => p
          }

          /*
           * Drop default value and 'hidden' annotation
           * in the parameter-list of the inner function.
           */
          val innerParams = parameters map {
            case Hidden(mods, name, tpe, _) =>
              q"$mods val $name: $tpe = $EmptyTree"
            case Normal(p, _) => p
          }

          /*
           * Pass default value of hidden parameters and
           * propagate other parameters to inner function.
           */
          val args = parameters map {
            case Hidden(_, _, _, default) => default
            case Normal(_, name)          => q"$name"
          }

          q"""
          def $funName[..$ptys](..$outerParams): $retTy = {
            def $funImplName(..$innerParams): $retTy = $newBody

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
