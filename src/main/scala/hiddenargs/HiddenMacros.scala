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

  /**
   * Analyze parameters marked with 'hidden'.
   *
   * Show an error if
   * - a 'hidden' parameter has no default value or
   * - a 'hidden' parameter is implicit.
   *
   * If no error was shown, remove the flag `Flag.DEFAULTPARAM`
   * from the parameters modifiers and the annotation 'hidden'.
   */
  private def hiddenParameter(tree: Tree,
                              mods: Modifiers,
                              name: TermName,
                              tpe: Tree,
                              default: Tree): Param = {

    def hiddenParamError(msg: String) = {
      c.error(tree.pos, s"Hidden function parameter '${name.decodedName.toString}' $msg!")
      Normal(tree, name)
    }
    def needsDefaultValue() = hiddenParamError("needs a default value")
    def cantBeImplicit()    = hiddenParamError("can't be implicit")

    if (default.isEmpty) {
      needsDefaultValue()
    } else if (mods.hasFlag(Flag.IMPLICIT)) {
      cantBeImplicit()
    } else {
      val noDefaultParamFlag =
        if (mods hasFlag Flag.DEFAULTPARAM) removeFlag(mods, Flag.DEFAULTPARAM)
        else mods
      val noHiddenAnnotation =
        noDefaultParamFlag.mapAnnotations(_ filterNot (isAnnotation[HiddenAnnot]))
      Hidden(noHiddenAnnotation, name, tpe, default)
    }
  }

  private def paramInfos(fundef: Tree, plists: List[List[Tree]]): (List[List[Param]], Boolean) = {
    val paramLists = plists map {
      _ map {
        case t @ q"$mods val $name: $tpe = $default" if hasAnnotation[HiddenAnnot](mods) =>
          hiddenParameter(t, mods, name, tpe, default)
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

  /**
   * Remove default arguments, which were marked with 'hidden',
   * keep every other parameter.
   */
  private def outerParameterLists(paramLists: List[List[Param]]): List[List[Tree]] =
    paramLists map {
      _ collect {
        case Normal(p, _) => p
      }
    }

  /**
   * Drop default values of parameters, which were marked with 'hidden',
   * keep every other parameter.
   */
  private def innerParameterLists(paramLists: List[List[Param]]): List[List[Tree]] =
    paramLists map {
      _ map {
        case Hidden(mods, name, tpe, default) =>
          q"$mods val $name: $tpe = $EmptyTree"
        case Normal(p, _) => p
      }
    }

  /**
   * Pass default value of hidden parameters and
   * propagate other parameters to inner function.
   */
  private def argumentsForInnerFunction(paramLists: List[List[Param]]): List[List[Tree]] =
    paramLists map {
      _ collect {
        case Hidden(_, _, _, default)                   => default
        case Normal(p, name) if !isImplicitParameter(p) => q"$name"
      }
    } filterNot (_.isEmpty)

  private def rewriteFunction(tree: Tree,
                              mods: Modifiers,
                              funName: TermName,
                              ptys: List[TypeDef],
                              params: List[List[ValDef]],
                              retTy: Tree,
                              funBody: Tree): Tree = {

    val (paramLists, shouldTransform) = paramInfos(tree, params)

    if (shouldTransform) {
      val funImplName     = TermName(funName.decodedName.toString() + "_impl")
      val newBody         = replaceCall(funBody, funName, funImplName)
      val outerParamLists = outerParameterLists(paramLists)
      val innerParamLists = innerParameterLists(paramLists)
      val args            = argumentsForInnerFunction(paramLists)

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
        rewriteFunction(tree, mods, funName, ptys, params, retTy, funBody)
      case t =>
        c.error(t.pos, "Unsupported use of annotation 'hiddenargs'!")
        t
    }

    q"{..$res}"
  }

}
