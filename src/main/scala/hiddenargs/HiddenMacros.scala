package hiddenargs

import scala.reflect.macros.whitebox.Context
import scala.collection.mutable.ListBuffer
import java.io.Serializable

private[hiddenargs] class HiddenMacros(val c: Context) {
  import c.universe._

  private val flagValues = Seq(
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

  /**
   * Rebuild `mods`, but exclude flag `remove`
   */
  private def removeFlag(mods: Modifiers, remove: FlagSet): Modifiers = {
    var flags = NoFlags
    for (flag <- flagValues) {
      if ((mods hasFlag flag) && flag != remove) {
        flags |= flag
      }
    }

    Modifiers(
      flags,
      mods.privateWithin,
      mods.annotations)
  }

  private type HiddenAnnot = hiddenargs.hidden

  private sealed trait Param extends Product with Serializable
  private case class Normal(param: Tree, name: TermName) extends Param
  private case class Hidden(mods: Modifiers, name: TermName, tpe: Tree, default: Tree) extends Param

  private def hasAnnotation[T](mods: Modifiers)(implicit tag: c.TypeTag[T]): Boolean =
    mods.annotations exists (isAnnotation[T])

  private def isAnnotation[T](annot: Tree)(implicit tag: c.TypeTag[T]): Boolean = {
    val typedAnnot = c.typecheck(annot, silent = true)
    typedAnnot.nonEmpty && typedAnnot.tpe =:= tag.tpe
  }

  private def isImplicitParameter(param: Tree): Boolean = param match {
    case q"$mods val $_ : $tpe" => mods hasFlag Flag.IMPLICIT
    case _                      => false
  }

  private def paramInfos(fundef: Tree, plists: List[List[Tree]]): (List[List[Param]], Boolean) = {
    val paramLists = plists map {
      _ map {
        case t @ q"$mods val $name: $tpe = $default" if hasAnnotation[HiddenAnnot](mods) =>
          if (default.isEmpty) {
            val paramName = name.decodedName.toString
            c.error(t.pos, s"Hidden function parameter '$paramName' needs a default value!")
            Normal(t, name)
          } else if (mods.hasFlag(Flag.IMPLICIT)) {
            val paramName = name.decodedName.toString
            c.error(t.pos, s"Hidden function parameter '$paramName' can't be implicit!")
            Normal(t, name)
          } else {
            val newMods = removeFlag(mods, Flag.DEFAULTPARAM).mapAnnotations(_  filterNot (isAnnotation[HiddenAnnot]))
            Hidden(newMods, name, tpe, default)
          }
        case t @ q"$mods val $name: $tpe = $default" =>
          Normal(t, name)
        case p =>
          c.abort(p.pos, "Unsupported shape of parameter!")
      }
    }

    val hasHiddenParameters = paramLists exists {
      _ exists {
        case _: Hidden => true
        case _: Normal => false
      }
    }

    if (!hasHiddenParameters) {
      c.warning(fundef.pos, s"Annotation 'hiddenargs' was used but no parameter was marked as private using the annotation 'hidden'!")
    }

    (paramLists, hasHiddenParameters)
  }

  private def changeFunction(tree: Tree,
                             mods: Modifiers,
                             funName: TermName,
                             ptys: List[TypeDef],
                             params: List[List[ValDef]],
                             retTy: Tree,
                             funBody: Tree): Tree = {

    val (paramLists, shouldTransform) = paramInfos(tree, params)

    if (shouldTransform) {
      val funImplName = TermName(funName.decodedName.toString() + "_impl")
      val newBody = replaceCall(funBody, funName, funImplName)

      /*
       * Drop default arguments, which were marked
       * with 'hidden'.
       */
      val outerParamLists = paramLists map {
        _ collect {
          case Normal(p, _) => p
        }
      }

      /*
       * Drop default value and 'hidden' annotation
       * in the parameter-list of the inner function.
       */
      val innerParamLists = paramLists map {
        _ map {
          case Hidden(mods, name, tpe, default) =>
            q"$mods val $name: $tpe = $EmptyTree"
          case Normal(p, _) => p
        }
      }

      /*
       * Pass default value of hidden parameters and
       * propagate other parameters to inner function.
       */
      val args = paramLists map {
        _ collect {
          case Hidden(_, _, _, default)                   => default
          case Normal(p, name) if !isImplicitParameter(p) => q"$name"
        }
      } filterNot (_.isEmpty)

      q"""
      def $funName[..$ptys](...$outerParamLists): $retTy = {
        $mods def $funImplName(...$innerParamLists): $retTy = $newBody

        $funImplName(...$args)
      }
      """
    } else {
      tree
    }
  }

  def transform(annottees: Tree*): Tree = {
    val res = annottees map {
      case tree @ q"$mods def $funName[..$ptys](...$params): $retTy = $funBody" =>
        changeFunction(tree, mods, funName, ptys, params, retTy, funBody)
      case t =>
        c.error(t.pos, "Unsupported use of annotation 'hiddenargs'!")
        t
    }

    q"{..$res}"
  }

}
