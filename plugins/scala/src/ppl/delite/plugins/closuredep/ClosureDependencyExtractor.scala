package ppl.delite.plugins.closuredep

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.ast.TreeBrowsers
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import nsc.symtab.Flags
import nsc.transform.Transform
import collection.mutable.{ListBuffer, HashSet}
//-Xplugin:/home/asujeeth/src/workspace/delite/plugins/jars/closuredep.jar

class ClosureDependencyExtractor(val global: Global) extends Plugin {
  import global._
  import definitions._

  val name = "closuredep"
  val description = "Extracts dependencies from closures"
  val warningPhase = name + "warn"
  val transformPhase = name + "fix"
  //val components = List[PluginComponent](WarningComponent)
  val components = List[PluginComponent](TransformComponent)
  //val components = List[PluginComponent](TransformComponent, WarningComponent)

  val DSLType = definitions.getClass("ppl.delite.core.DeliteDSLType").tpe

  val function_whitelist = List("untilconverged", "sumd_noplugin")
  val source_whitelist = List("LBPModel.scala")
  
  private object WarningComponent extends PluginComponent {
    val global: ClosureDependencyExtractor.this.global.type = ClosureDependencyExtractor.this.global
    val runsAfter = List[String](ClosureDependencyExtractor.this.transformPhase);
    //val runsAfter = List[String]("namer");
    val phaseName = ClosureDependencyExtractor.this.warningPhase

    /* the phase plugin only generates warnings; it does not transform the AST */
    def newPhase(_prev: Phase) = new ClosureDependencyPhase(_prev)

    class ClosureDependencyPhase(prev: Phase) extends StdPhase(prev) {
      override def name = ClosureDependencyExtractor.this.name
      def apply(unit: CompilationUnit) {
        //ClosureDependencyExtractor.this.global.treeBrowsers.create().browse(unit.body)
        //println("ClosureDependencyExtractor: examining unit..")
        
        // step 1: find Function tags inside DeliteDSL methods
        for (tree @ Apply(_,List(Function(_,_))) <- unit.body){
          if (tree.symbol.owner.tpe <:< DSLType){
            for (c <- tree.children){ c match {
              case func: Function =>
                // step 2a: find any nested references to objects with type DeliteDSLType (issue warning)
                for (ref @ Select(qual,_) <- func){
                  if (qual.tpe <:< DSLType && qual.symbol.owner != func.symbol){
                    unit.warning(qual.pos, "reference to DeliteDSLType inside likely op closure")
                  }
                  // step 3a: do something about it
                }
                // step 2b: find any nested references to objects declared outside of the closure (issue warning)
                for (ref @ Ident(_) <- func){
                  if (ref.symbol.isVariable && ref.symbol.owner != func.symbol){
                    unit.warning(ref.pos, "non-local variable captured inside likely op closure")
                  }
                }
              case _ =>
            } // match
            } // for children
          } // if DSLtype
        } // outer apply

      }

    }
  } // PluginComponent

  private object TransformComponent extends PluginComponent with Transform {
    val global: ClosureDependencyExtractor.this.global.type = ClosureDependencyExtractor.this.global
    val runsAfter = List[String]("namer");
    val phaseName = ClosureDependencyExtractor.this.transformPhase

    def newTransformer(unit: CompilationUnit) = new ClosureDependencyTransformer(unit)

    class ClosureDependencyTransformer(unit: CompilationUnit) extends Transformer {
      val funcTransformer = new FunctionTransformer(unit)
      val optiml = definitions.getClass("ppl.delite.dsl.optiml.appinclude")

      override def transform(tree: Tree) : Tree = {
        // for debugging only
        //ClosureDependencyExtractor.this.global.treeBrowsers.create().browse(tree)
        //println("ClosureDependencyExtractor: examining unit..")

        if (source_whitelist contains unit.source.file.name) return tree

        tree match {
          // step 1: find Function tags inside DeliteDSL methods
          case tr @ Apply(_,List(Function(_,_))) =>
            //unit.warning(tr.pos, "tr.symbol.owner.name: " + tr.symbol.owner.name.toString + " | optiml: " + optiml.name.toString + " | "
            //             + (tr.symbol.owner.name == optiml.name).toString + " | tr.symbol.decodedName " + tr.symbol.decodedName )
            //ClosureDependencyExtractor.this.global.treeBrowsers.create().browse(tree)
            if (!(function_whitelist contains tr.symbol.decodedName) && ((tr.symbol.owner.tpe <:< DSLType) || (tr.symbol.owner.name == optiml.name))) {
              funcTransformer.transform(tr)
            }
            else{
              super.transform(tr)
            }

          case _ => super.transform(tree)
        }
      }

    } // ClosureDependencyTransformer

    class FunctionTransformer(unit: CompilationUnit) extends Transformer {
      val ppl = definitions.getModule("ppl")
      val delite = definitions.getModule("ppl.delite")
      val core = definitions.getModule("ppl.delite.core")
      val deliteFunc = definitions.getModule("ppl.delite.core.DeliteFunc")
      val deliteFuncApply = definitions.getMember(deliteFunc, nme.apply)

      val pplObjTpe = singleType(ppl.tpe.prefix, ppl)
      val deliteObjTpe = singleType(pplObjTpe, delite)
      val coreObjTpe = singleType(deliteObjTpe, core)
      val deliteFuncObjTpe = singleType(coreObjTpe, deliteFunc)

      override def transform(tree: Tree) : Tree = {
        //ClosureDependencyExtractor.this.global.treeBrowsers.create().browse(tree)
        tree match {
        case func @ Function(_,_) =>
          val localFuncs : HashSet[Symbol] = HashSet(func.symbol)
          val foundDepSyms : HashSet[Symbol] = HashSet()

          for (nested @ Function(_,_) <- func) localFuncs += nested.symbol
          
          // step 2a: find any nested references to objects declared outside of the closure (issue warning)
          // TODO: do this transformation
          //for (ref @ Ident(_) <- func){
          //  if (ref.symbol.isVariable && ref.symbol.owner != func.symbol){
          //    unit.warning(ref.pos, "transforming non-local variable captured inside likely op closure")
          //  }
          //}

          val deps : ListBuffer[Tree] = ListBuffer[Tree]()

          // step 2b: find any nested references to objects with type DeliteDSLType (issue warning)
          for (qual @ Ident(_) <- func){
            maybeAddDep(deps, qual, localFuncs, foundDepSyms)
          }
          for (ref @ Select(qual,_) <- func){
            maybeAddDep(deps, ref, localFuncs, foundDepSyms)
            maybeAddDep(deps, qual, localFuncs, foundDepSyms)
          }
          // step 3b: convert function parameter to DeliteFunc with dependencies deps
          if (deps.length > 0) createDeliteFunc(func, deps)
          else func

        case _ => super.transform(tree)
      } // transform
      }

      def maybeAddDep(deps: ListBuffer[Tree], qual: Tree, localFuncs: HashSet[Symbol], foundDepSyms: HashSet[Symbol]){
        /*
        if (qual.symbol != null){
          unit.warning(qual.pos, "qual: " + qual.toString + " | symbol: " + qual.symbol.toString + " (isSourceMethod: " + qual.symbol.isSourceMethod + ") | type: " +
                       qual.symbol.tpe.toString + "| tree.type: " + qual.tpe.toString )
        }
        */
        if (qual.symbol != null && qual.symbol.tpe.resultType <:< DSLType && (!qual.symbol.isSourceMethod || qual.symbol.isGetter) && !(localFuncs contains qual.symbol.owner)){
          //unit.warning(qual.pos, "qual: " + qual.toString + " | symbol: " + qual.symbol.toString + " (isSourceMethod: " + qual.symbol.isSourceMethod + ") | type: " +
          //             qual.symbol.tpe.toString + "| tree.type: " + qual.tpe.toString + " | isGetter: " + qual.symbol.isGetter )          
          if (!(foundDepSyms contains qual.symbol)){
            var _independentQual = true
            for (qapp @ Apply(_,_) <- qual) {
              // this reference requires an indexed access for which we do not know the index statically
              _independentQual = false
              unit.warning(qual.pos, "could not transform reference to DeliteDSLType inside likely op closure -- unknown index required.")
            }
            for (qident @ Ident(_) <- qual){
              if (localFuncs contains qident.symbol.owner){
                // this reference is a member of a locally declared value           
                _independentQual = false
              }
            }
            if (_independentQual){
              //unit.warning(qual.pos, "qual: " + qual.toString + " | symbol: " + qual.symbol.toString + " (isSourceMethod: " + qual.symbol.isSourceMethod + ") | type: " +
              //         qual.symbol.tpe.toString + "| tree.type: " + qual.tpe.toString + " | isGetter: " + qual.symbol.isGetter )
              unit.warning(qual.pos, "transforming reference to DeliteDSLType inside likely op closure")
              deps += qual
              foundDepSyms += qual.symbol
            }
          }
        }
      }

      def createDeliteFunc(func: Function, deps: ListBuffer[Tree]) : Tree = {
        //ClosureDependencyExtractor.this.global.treeBrowsers.create().browse(func)
        // determines which DeliteFunc to construct
        val deliteFuncApplyChoices =
          deliteFuncApply.alternatives.filter( a => a.tpe.typeParams.length == func.tpe.typeArgs.length)
        if (deliteFuncApplyChoices.length > 1){
          unit.warning(func.pos, "multiple DeliteFunc implementations found, defaulting to first available")
        }
        if (deliteFuncApplyChoices.length < 1){
          unit.error(func.pos, "no suitable DeliteFunc implementation found for function")
          return func
        }
        val deliteFuncApplySpec = deliteFuncApplyChoices(0)
        val deliteFuncTpe = deliteFuncApplySpec.tpe
        val appliedDeliteFuncTpe = MethodType(deliteFuncTpe.params, deliteFuncTpe.finalResultType)
          .instantiateTypeParams(deliteFuncTpe.typeParams, func.tpe.typeArgs)

        val ret =
          global.Apply(
            global.TypeApply(
              global.Select(
                global.Select(
                  global.Select(
                    global.Select(
                      global.Ident(ppl).setType(pplObjTpe),
                      delite
                    ).setType(deliteObjTpe),
                    core
                  ).setType(coreObjTpe),
                  deliteFunc
                ).setType(deliteFuncObjTpe),
                deliteFuncApplySpec
              ).setType(deliteFuncTpe),

              func.tpe.typeArgs map { typ => TypeTree(typ) }
            ).setType(appliedDeliteFuncTpe),

            List(func) ++ deps
          ).setType(appliedDeliteFuncTpe.finalResultType)

        //ClosureDependencyExtractor.this.global.treeBrowsers.create().browse(ret)
        ret
      }

    } // FunctionTransformer

  } // PluginComponent

} // ClosureDependencyExtractor

