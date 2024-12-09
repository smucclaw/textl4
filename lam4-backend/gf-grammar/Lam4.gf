abstract Lam4 = {
  flags startcat = S ;
  cat
    S ;
    TypeDecl ;
    RowTypeDecl ;
    [RowTypeDecl]{0} ;
    Metadata ;

    Expr ;
    [Expr]{0} ;
    UnaryOp ;
    BinOp ;
    Name ;
    [Name]{0} ;

    -- Special values for aggregation
    ListOp ;
    LExpr ;
    [LExpr]{2} ;

    -- (condition, value)
    -- in order to flatten nested if-then-elses into if-elif-elif-…-else
    IfThen ;
    [IfThen]{2} ;
  fun
    -- Placeholder, or skip some constructs?
    EmptyS : S ;
    TypeDeclS : (id : String) -> TypeDecl -> S ;
    EvalS,
    EvalWhetherS,
    ExprS : (id : String) -> Expr -> S ;
    AssignS : (id : String) -> Name -> Expr -> S ;
    LetIsTrue : (id : String) -> Name -> Expr -> S ;
    AtomicConcept : (id : String) -> Name -> S ;

    -- Metadata
    MkMetadata : String -> Metadata ;
    NoMetadata : Metadata ; -- empty

    -- Type declarations
    MkRowTypeDecl : Metadata -> (fld : Name) -> (typ : Name) -> RowTypeDecl ;
    MkRowDecl : Metadata -> (fld : Name) -> RowTypeDecl ; -- no type

    MkTypeDecl : Metadata -> (name : Name) -> [RowTypeDecl] -> TypeDecl ;

    -- Expressions
    MkName : String -> Name ;
    Lit,
    QuoteVar,
    Var : Name -> Expr ;

    -- the following correspond to List and ListExpr in Lam4 AST,
    -- named differently because of a bug in GF,
    -- see https://github.com/GrammaticalFramework/gf-core/issues/163
    ConjExpr : [Expr] -> Expr ; -- construct a list

    coerceListExpr : Expr -> LExpr ;
    ApplyListOp : ListOp -> [LExpr] -> Expr ;

    ListAnd, ListOr : ListOp ;

    Unary   : UnaryOp -> Expr -> Expr ;
    VerboseBinExpr, -- newline + quotes around args
    BinExpr : BinOp -> Expr -> Expr -> Expr ;

    UnaryMinusExpr : Expr -> Expr ;
    Round : (expr, prec : Expr) -> Expr ;
    Default : (val, default : Expr) -> Expr ;

    IfThenElse : Expr -> Expr -> Expr -> Expr ;

    -- Flatten nested If Then Elses
    FirstIfThen : Expr -> Expr -> IfThen ;
    MiddleIfThen : Expr -> Expr -> IfThen ;
    NilIfThen : Expr -> IfThen ; -- the original IfThenElse
    Elif: [IfThen] -> Expr ;
    {-

      IF    i's `the business's number of past and current clients`  is larger than
       * 10.0: => the business's client bonus factor is 1.5
       * 4.0  => the business's client bonus factor is 1.35
       * 1.0  => the business's client bonus factor is 1.2
       * ELSE => the business's client bonus factor is 1.0

    -}

    -- For linearizing functions that have a NLG annotation
    FunApp1 : (description : String) -> (arg : Expr) -> Expr ;
    FunApp2 : (desc1 : String) -> (arg1 : Expr) -> (desc2 : String) -> (arg2 : Expr) -> Expr ;

    -- For linearizing functions that don't have NLG annotation
    FunApp : Expr -> [Expr] -> Expr ;
    -- Record : (Row Expr) -> Expr ;               -- record construction
    OnlyFieldProject,
    Project : Expr -> Name -> Expr ;             -- record projection
    Fun : (funname : Name) -> Metadata -> (args : [Name]) -> Expr -> Expr ;  -- Function
    Sig : [Name] -> [Expr] -> Expr ; -- TODO: what is this? only `Sig [] []` present in royalflush data

    {- TODO: this is used in context like

       LET { foo
           , bar
           , baz
           }
       IN { expression that uses definitions
          , another expression
          , yet another
          … }

       as AST it looks like
         Let (foo
           Let (bar
             Let baz )))

       transform into:

       [all variables]

       [all Exprs]

       and linearize as
       -- Definitions ---
       foo = …
       bar = …
       baz = …

       -- Expressions --
       if they are just like
       `the business eligibility text` = `the business eligibility text`,
       then don't print them.
    -}
    Let : S -> Expr -> Expr ;
    Record : Name -> Expr -> Expr ;
    -- StatementBlock :  (NonEmpty Statement)

    NormIsInfringed : Name -> Expr ;             -- NormIsInfringed NameOfNorm.
                                                   -- This is a predicate that checks if @nameOfNorm@ is violated (users can supply unique identifiers for Deontics and IfThenOtherwise statements that contain a Deontic)


    Predicate : (predname : Name) -> Metadata -> (args : [Name]) -> Expr -> Expr ;            -- Differs from a function when doing symbolic evaluation. Exact way in which they should differ is WIP.

    PredApp : (pred : Expr) -> (args : [Expr]) -> Expr ;

    -- Aggregation of multiple PredApps being applied to the same argument(s).
    PredAppMany : BinOp -> (preds : [Expr]) -> (args : [Expr]) -> Expr ;
    Fold : Expr -> Expr -> Expr -> Expr ;

    -- When generating natural language for some file that defines a bunch of stuff like cons, map, filter,
    -- apply this function instead to keep it in the AST
    -- but skip linearization.
    KnownFunction : Name -> Expr ;


    -- Unary and binary operators
    Not,
    Floor,
    Ceiling,
    UnaryMinus,
    IntegerToFraction : UnaryOp ;

    Or : BinOp ;
    And : BinOp ;
    Plus : BinOp ;
    Minus : BinOp ;
    Modulo : BinOp ;   -- ^ (integer) remainder
    Mult : BinOp ;
    Divide : BinOp ;
    Lt : BinOp ;
    Le : BinOp ;
    Gt : BinOp ;
    Ge : BinOp ;      -- ^ greater-than-or-equal
    Eq : BinOp ;      -- ^ equality (of Booleans, numbers or atoms)
    Ne : BinOp ;      -- ^ inequality (of Booleans, numbers or atoms)
}