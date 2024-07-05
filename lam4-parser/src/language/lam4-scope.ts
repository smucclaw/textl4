import type {AstNode, LangiumCoreServices} from "langium";
import { DefaultScopeProvider, EMPTY_SCOPE, ReferenceInfo, Scope } from "langium";
import { Logger } from "tslog";
import { BinExpr, SigDecl, isBinExpr } from "./generated/ast.js";
import { getSigAncestors, synth, TypeEnv } from "./type-system/infer.js";
import { isSigTTag } from "./type-system/type-tags.js";
import { isJoinExpr } from "./lang-utils.js";

const scopeLogger = new Logger({ 
  name: "scoper",
  prettyLogTemplate: "{{name}}  ", });

export class Lam4ScopeProvider extends DefaultScopeProvider {

  constructor(services: LangiumCoreServices) {
      super(services);
  }

  /**
   * General notes re how scopes are computed in Langium:
   * - Cross-references are resolved *only after* local scopes have been computed. I.e., can't rely on being able to access cross-references in the scope computaton phase. See https://langium.org/docs/reference/document-lifecycle/
   * - "A symbol in the precomputed scopes is reachable from a specific cross-reference if it is associated with a direct or indirect container of that reference."
   * - "The default implementation of the ScopeComputation service attaches the AstNodeDescription of every symbol to its direct container. This means that the container holds information about which named nodes are nested inside of it. You can override this default behavior to change the position where a symbol is reachable, or to change the name by which it can be referenced"
   */
  override getScope(context: ReferenceInfo): Scope {
    scopeLogger.trace(`(getScope) ctx property: ${context.property}`);
    scopeLogger.trace(`           container: ${context.container.$type}; its parent: ${context.container.$container?.$type}`);

    /*
    x `s`: getScope: ctx property: value
                        container with type Ref; its parent container has type BinExpr
    x `s` a: getScope: ctx property: value
             container with type Ref; its parent container has type Ref
    */   
   
    const self = context.container;
    const isRightmostChildOfJoin = self.$type === "Ref" && isJoinExpr(self.$container) && (self.$container as BinExpr).right === self;
    if (isRightmostChildOfJoin) {
      scopeLogger.trace(`(Scope-if) Ref ${context.reference.$refText} is right child of a join`);
      const parent = self.$container as BinExpr;

      scopeLogger.trace(`           left sib is ${parent.left.$type}`);
      const typeOfLeft = synth(new TypeEnv(), parent.left);
      // IMMED-TODO: Ah, so we will need to make sure inferType BinExpr where BinExpr is a join works, in order for things like  x `s` a `s` b to work!
      scopeLogger.trace(`           left sib ::`, typeOfLeft.toString());
      const returnScope = isSigTTag(typeOfLeft) ? 
                          this.scopeSigMembers(typeOfLeft.getSig()) : EMPTY_SCOPE
      // When the target of our member call isn't a sig
      // This means it is either a primitive type or a type resolution error
      // Simply return an empty scope
      return returnScope;
    }

    return super.getScope(context);
  }

  private scopeSigMembers(sig: SigDecl): Scope {
    const allRelations = getSigAncestors(sig).flatMap((s: SigDecl) => s.relations);
    scopeLogger.debug(`relations: ${allRelations.map(r => r.name)}`);
    return this.createScopeForNodes(allRelations);
  } 
}

