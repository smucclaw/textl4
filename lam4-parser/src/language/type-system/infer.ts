import { AstNode, Reference } from "langium";

import { 
    // isLiteral, Literal, 
    SigDecl, isSigDecl, isStringLiteral, StringLiteral, isBooleanLiteral, BooleanLiteral, NumberLiteral, isNumberLiteral, isVarDecl, isTypeAnnot, TypeAnnot, isRelation, Relation, isBuiltinType, BuiltinType, isCustomTypeDef, CustomTypeDef, isParamTypePair, isIfThenElseExpr, IfThenElseExpr, isComparisonOp, isBinExpr, BinExpr, FunDecl, isFunDecl, 
    isFunctionApplication,
    FunctionApplication,
    PredicateDecl,
    isPredicateDecl,
    isParam,
    Ref,
    Join} from "../generated/ast.js";
import { TypeTag, ErrorTypeTag, StringTTag, IntegerTTag, isBooleanTTag, FunctionTTag, isFunctionTTag, PredicateTTag, isPredicateTTag, SigTTag, BooleanTTag, FunctionParameter, isErrorTypeTag, isSigTTag, isRelationTTag, RelationTTag} from "./type-tags.js";

import { 
    // Either, Failure, Success, 
    zip } from "../../utils.js"
import { match, P } from 'ts-pattern';
import { Logger } from "tslog";


/*
TODOs: 
* Handle predicates etc -- pred app the main thing left
* Add more typechecking for the Join / record access later
* isSubtypeOf
*/

const typecheckLogger = new Logger({ 
    name: "tinfer",
    minLevel: 1,
    prettyLogTemplate: "{{name}}  {{logLevelName}}  "});


const ARITH_OPERATORS = ['OpPlus', 'OpMinus', 'OpMult', 'OpDiv'];

type NodeTypePair = {node: AstNode, type: TypeTag};
// Branded<{node: AstNode, type: TypeTag}, "NodeTypePair">;

export class TypeEnv {
    private readonly envMap: Map<AstNode, TypeTag>;

    constructor(initialEnvMap = new Map()) {
        this.envMap = initialEnvMap;
    }

    lookup(node: AstNode) {
        return this.envMap.get(node) ?? null;
    }

    set(node: AstNode, type: TypeTag) {
        this.envMap.set(node, type);
    }

    /* Returns a *new* env that's extended with the input node-type pairs */
    extendWith(nodeTypePairs: NodeTypePair[]) {
        const newPairMap = new Map(nodeTypePairs.map(pair => [pair.node, pair.type]));
        const newEnvMap = new Map([...this.envMap, ...newPairMap]);
        return new TypeEnv(newEnvMap);
    }
}


export function synth(env: TypeEnv, term: AstNode): TypeTag {

    if (!term) return new ErrorTypeTag(term, "Cannot infer type for a node that's falsy");

    const existing = env.lookup(term);
    if (existing) {
        return existing;
    } else {
        return synthNewNode(env, term);
    }
}

// TODO: this is buggy / causing "Maximum call stack size exceeded" when it's called from functionTTagFromFuncDecl
const check = (env: TypeEnv, term: AstNode, type: TypeTag): TypeTag =>
    match(term)
        .with(P.when(isBinExpr),
            (binE) => {

                function checkChildrenHaveSameType(env: TypeEnv, expr: BinExpr, type: TypeTag): TypeTag {
                    const leftType = check(env, expr.left, type);
                    const rightType = check(env, expr.right, type);
                    const typesCheck = [leftType, rightType].every(typeTag => typeTag.sameTypeAs(type));
                    return typesCheck ? type : new ErrorTypeTag(expr, `Type error in binary expression. Expected type to be ${type}, but got ${leftType} and ${rightType}`);
                }
                return checkChildrenHaveSameType(env, binE, type);
            })
        .with(P.when(isFunDecl),
            (fundecl: FunDecl) => {
                if (!isFunctionTTag(type)) return new ErrorTypeTag(fundecl, "Function declaration must be annotated with a function type");

                const funcParamTypePairs: FunctionParameter[] = type.getParameters();
                const funcRetType = type.returnType;
                const newExtendedEnv = env.extendWith(
                                funcParamTypePairs.map(pair => ({node: pair.param, 
                                                                type: pair.type} as NodeTypePair)));
                return check(newExtendedEnv,
                             fundecl.body, 
                             funcRetType);
            })
        .with(P.when(isPredicateDecl),
            (predDecl: PredicateDecl) => {
                typecheckLogger.trace("PredicateDecl TODO");
                return new ErrorTypeTag(predDecl, "TODO");
                // if (!isPredicateTTag(type)) return new ErrorTypeTag(predDecl, 
                //                             "Predicate decl must be annotated with a predicate type");

                // const newExtendedEnv = env.extendWith(
                //         type.getParameters().map(pair => ({node: pair.param, 
                //         type: pair.type} as NodeTypePair)));
                        
                // return check(newExtendedEnv, predDecl.body, new BooleanTTag());
            })
        .with(P.when(isIfThenElseExpr), 
            (ite: IfThenElseExpr) => { 
                const iteChecksPass = isBooleanTTag(synth(env, ite.condition)) 
                                        && synth(env, ite.then).sameTypeAs(type)
                                        && synth(env, ite.else).sameTypeAs(type); 
                // TODO: Could do more error checking and more detailed error messages
                return iteChecksPass ? type : new ErrorTypeTag(ite, "Type error in if-then-else expression (a more helpful error msg is possible with more work)");
            })
        
        .otherwise(term => {
            let termType = synth(env, term);
            return termType.sameTypeAs(type) ? type : new ErrorTypeTag(term, "Type error");
            // TODO in the future: isSubtype(termType, type) ? type : new ErrorTypeTag(term, "Type error");
        });


function inferTypeAnnot(env: TypeEnv, typeAnnot: TypeAnnot): TypeTag {
    function builtinTypeAnnotToTypeTag(typeAnnot: BuiltinType) {
        const annot = typeAnnot.annot;
        switch (annot) {
            case "String":
                return new StringTTag();
            case "Integer":
                return new IntegerTTag();
            case "Boolean":
                return new BooleanTTag();
            default:
                return annot as never;
        }
    }

    function customTypeAnnotToTypeTag(typeAnnot: CustomTypeDef) {
        const customType = typeAnnot.annot.ref;
        if (!customType) {
            return new ErrorTypeTag(typeAnnot, `Type annotation ${typeAnnot.annot} not found because of an issue with linking`);
        }
        return new SigTTag(customType as SigDecl);
    }


    // Get the type annot
    if (isBuiltinType(typeAnnot)) {
        return builtinTypeAnnotToTypeTag(typeAnnot);
    } else if (isCustomTypeDef(typeAnnot)) {
        return customTypeAnnotToTypeTag(typeAnnot);
    } else {
        return new ErrorTypeTag(typeAnnot, `Type annotation ${typeAnnot} not recognized`);
    }
}

/*
Some intuition on bidirectional typechecking, for those unfamiliar with it:
-----------------------------------------------------------------------------
    How would
       infer ( (f 1) + 2 )
    go?

    1. Infer that (f 1) and 2 should be integers
    2. Check whether (f 1) is an integer
    3. => Try to -- infer the type of (f 1) ---.
            * (Infer fun app) 
                    Try to infer quid `f`; if it's inferred to be a function type of form (tfun a b), check that the actual argument `1` has the inferred type `a`
                    If `f` inferred to be non-function, throw 'expected function erorr'
                    If error, pass error along

                    Comment: how `f` is used is not enough to allow you to infer the type of `f`, on this algo
        * If we are able to infer the type of (f 1), call this inferred_type, 
            check whether inferred_type equal to integer.  

    Other resources:
    * https://www.reddit.com/r/ProgrammingLanguages/comments/v3z7r8/comment/ib32xpr/ points out that this saves you from having to 
    annotate function args when using higher order functions
*/

function inferBinExpr(env: TypeEnv, expr: BinExpr): TypeTag {
    typecheckLogger.trace(`[inferBinExpr]`);

    const isBoolExpr = isComparisonOp(expr.op) || (['OpAnd', 'OpOr'].includes(expr.op.$type));
    let binType: TypeTag;
    if (isBoolExpr) {
        // TODO: Not sure if should check for whether the arguments are indeed boolean exprs here, or in `lam4-validator` 
        binType = new BooleanTTag();
        return check(env, expr, binType);
    } else if (ARITH_OPERATORS.includes(expr.op.$type)) {
        binType = new IntegerTTag();
        return check(env, expr, binType);
    } else {
        return new ErrorTypeTag(expr, "Type of binary expression cannot be inferred");
    }
}


function predicateTTagFromPredDecl(env: TypeEnv, predDecl: PredicateDecl): TypeTag {
    const paramTypePairs = predDecl.params.map(pair => ({param: pair.param, type: inferTypeAnnot(env, pair.type)} as FunctionParameter));
    const predicateType = new PredicateTTag(paramTypePairs);
    return predicateType;
}

// TODO-immed!
function functionTTagFromFuncDecl(env: TypeEnv, fundecl: FunDecl): TypeTag {
    const paramAndRetTypes = fundecl.types.map(synth.bind(undefined, env));
    const argTypes = paramAndRetTypes.slice(0, -1);
    const retType = paramAndRetTypes[paramAndRetTypes.length - 1];

    if (argTypes.length !== fundecl.params.length) return new ErrorTypeTag(fundecl, "Number of parameters and parameter types do not match");
    
    const argTypeTags: FunctionParameter[] = 
          zip(fundecl.params, argTypes)
          .map(paramTypeTuple => ({param: paramTypeTuple[0], type: paramTypeTuple[1]} as FunctionParameter));

    const funcType = new FunctionTTag(argTypeTags, retType);
    typecheckLogger.trace(`[functionTTagFromFuncDecl] synth'd ${funcType.toString()}`);
    return funcType;
}

function synthRef(env: TypeEnv, ref: Ref): TypeTag {
    function getRefInfoForUser(ref: Ref): string {
        return `reftxt: ${ref.value.$refText}; linking error?: ${ref.value.error?.message}`
    }
    
    typecheckLogger.trace(`[synthRef] reftxt: ${ref.value.$refText}`);

    if (ref.value.ref) return synth(env, ref.value.ref);

    typecheckLogger.debug(`Ref ${ref.value.$refText} has not been linked!`)
    return new ErrorTypeTag(ref, `Can't typecheck ref because reference can't be resolved. ${getRefInfoForUser(ref)}`)
}

/**
 * Key Precondition: If the join is well-typed, then the reference of the leftmost child of the join is already resolved. 
 * This is guaranteed by the structure of the Join construct in the grammar: 
 * because the depth of the leftmost child will not exceed that of the right, the scoper/linker will visit the leftmost child first
 * (and so, if the leftmost child is a Ref that can be resolved by the default linker, the Ref's reference will have been resolved) 
 */
function synthJoin(env: TypeEnv, term: Join): TypeTag {
    typecheckLogger.debug(`Trying to synth left of join: ${term.left.$type}`);
    const typeOfLeft = synth(env, term.left);
    typecheckLogger.debug(`left of join :: ${typeOfLeft.toString()}`);

    const rightRefName = term.right.value.$refText;
    
    // With the type of the left sibling, we can infer what the type of `left `s` right` is
    if (isSigTTag(typeOfLeft)) {
        const sig = typeOfLeft.getSig();
        const matchingRelation: Relation | undefined = sig.relations.find(relNode => relNode.name === rightRefName);
        if (matchingRelation) {
            typecheckLogger.debug("matchingRelation:", matchingRelation.name);
            const relationType = synth(env, matchingRelation);
            typecheckLogger.debug(`(matchingRelation) ${matchingRelation.name} :: ${relationType.toString()}`);
            if (!isRelationTTag(relationType)) return new ErrorTypeTag(matchingRelation, `Relation should have a RelationTTag, but it instead has ${relationType.toString()}`);

            const joinedType = relationType.joinOnLeft(typeOfLeft);
            typecheckLogger.debug(`joinedType: ${joinedType}`);
            return joinedType ?? new ErrorTypeTag(term, "join should have worked");
            // TODO: Do the checking of `ref`'s type in the validator
        } else {
            return new ErrorTypeTag(term, `Relatum ${rightRefName} not found in ${sig.name}`)
        }
    } else {
        return new ErrorTypeTag(term.left, `The type of something to the left of a join / field access operator should be a Concept, and not a ${typeOfLeft.toString()}`)
    }
}

export function synthNewNode(env: TypeEnv, term: AstNode): TypeTag {
    typecheckLogger.trace(`--- [synthNewNode] -- term: ${term.$type}`);

    let typeTag: TypeTag = new ErrorTypeTag(term, 'Could not infer type');

    // Ref has to come first, on pain of "RangeError: Maximum call stack size exceeded"
    // also, can't use the reflection-using `isRef` without getting that error with other things that can contain Refs as children 
    // (probably has to do with how the ref resolution hasn't fully completed at this stage)
    if (term.$type === "Ref") {
        typeTag = synthRef(env, term as Ref);
    } else if (term.$type === "Join") {
        typeTag = synthJoin(env, term as Join);

    // Constructs whose types can be easily read off the term
    } else if (isStringLiteral(term)) {
        typeTag = new StringTTag(term as StringLiteral);
    } else if (isBooleanLiteral(term)) {
        typeTag = new BooleanTTag(term as BooleanLiteral);
    } else if (isNumberLiteral(term)) {
        typeTag = new IntegerTTag(term as NumberLiteral);
    } else if (isVarDecl(term)) {
        typeTag = term.varType ? synth(env, term.varType) : synth(env, term.value);

    } else if (isParam(term)) {
        typecheckLogger.trace(`\t\t(isParam) trying to synth param's parent, ${term.$container.$type}`);
        const typeOfParent = synth(env, term.$container);
        typecheckLogger.trace(`\t\t(isParam) typeOfParent: ${typeOfParent.toString()}`);
        if (isFunctionTTag(typeOfParent)) {
            const paramType = typeOfParent.getTypeOfParam(term);
            typecheckLogger.trace(`paramType: ${paramType?.toString()}`);
            typeTag = paramType ?? new ErrorTypeTag(term, "param either not a param of function or lacks a type annotation");
        } else {
            typeTag = new ErrorTypeTag(term, "param either not a param of function or lacks a type annotation");
        }
    } else if (isParamTypePair(term)) {
        typeTag = synth(env, term.type);
    } else if (isTypeAnnot(term)) {
        typeTag = inferTypeAnnot(env, term);
    
    } else if (isSigDecl(term)) {
        typeTag = new SigTTag(term);
    } else if (isRelation(term)) {
        typeTag = isSigDecl(term.$container) ?
            new RelationTTag(term, (synth(env, term.$container) as SigTTag), synth(env, term.relatum)) :
            new ErrorTypeTag(term, "Relation should have a Concept as its parent");

    } else if (isBinExpr(term)) {
        typeTag = inferBinExpr(env, term);
    } else if (isFunDecl(term)) {
        typeTag = functionTTagFromFuncDecl(env, term);
    } else if (isFunctionApplication(term)) {
        const inferredFuncType = synth(env, term.func);
        if (isFunctionTTag(inferredFuncType)) {
            // Check that Γ |- arg : expected type, for each of the (arg, expected type) pairs
            const inferredArgTypes = inferredFuncType.getParameters().map(p => p.type);

            let checkPassed = true;
            for (const [arg, expectedType] of zip(term.args, inferredArgTypes)) {
                const checkRetType = check(env, arg as AstNode, expectedType as TypeTag);
                if (isErrorTypeTag(checkRetType)) {
                    typeTag = checkRetType;
                    checkPassed = false;
                }
            }
            if (checkPassed) {
                typeTag = inferredFuncType.returnType;
            }
        } else {
            typeTag = new ErrorTypeTag(term.func, "We expected a function type because of the function application, but this is not a function type");
        }

    } else if (isPredicateDecl(term)) {
        const predType = predicateTTagFromPredDecl(env, term);
        typeTag = check(env, term, predType);
    } else {
        typeTag = new ErrorTypeTag(term, `Type of term cannot be inferred`);
    }

    env.set(term, typeTag);
    return typeTag;
}

/** Get Sig ancestors via DFS */
export function getSigAncestors(sig: SigDecl): SigDecl[] {
    const seen = new Set<SigDecl>();
    const toVisit: SigDecl[] = [sig];

    while (toVisit.length > 0) {
        let next: SigDecl | undefined = toVisit.pop();
        if (!next) break; // TODO: temp hack cos TS can't narrow based on length out of box

        if (!seen.has(next)) {
            seen.add(next);
            next.parents.forEach((parent: Reference<SigDecl>) => { 
                if (parent.ref) toVisit.push(parent.ref) 
            });
        }
    }

    // Sets preserve insertion order
    const seenArr = Array.from(seen);
    typecheckLogger.silly('seenArr:', seenArr.map(sig => sig.name));

    return seenArr;
}


/*============= Error ================================ */
// class TCError {
//     readonly tag = "TCError";
//     readonly node: AstNode;
//     readonly message: string;

//     constructor(node: AstNode, message: string) {
//         this.node = node;
//         this.message = message;
//     }

// }
// type CheckResult = Either<TCError, TypeTag>;

// TODO: This is hacky -- figure out what a better way of handling type errors is, after the first draft
// function tcErrorToTypeTag(tcError: TCError): TypeTag {
//     return new ErrorTypeTag(tcError.astNode, tcError.message);
// }

// const unwrapResult = (either: Either<TCError, TypeTag>): TypeTag =>
//      match(either)
//         .with({tag: "failure"}, (either) => tcErrorToTypeTag(either.value))
//         .with({tag: "success"}, (either) => either.value)
//         .exhaustive();
