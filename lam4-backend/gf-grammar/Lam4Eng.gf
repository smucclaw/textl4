concrete Lam4Eng of Lam4 = open Prelude, Coordination in {
  lincat
    -- TypeDecl
    -- RowTypeDecl
    [RowTypeDecl] = {s : Str ; isEmpty : IsEmpty} ;
    Metadata = LinMetadata ;
    ListExpr = ListX0 ;
    ListName = ListX0 ;
    BinOp = {s : Verbosity => Str} ;
    ListLExpr = LinLExpr ;
    ListOp = LinListOp ;

  param
    IsEmpty = Empty | NonEmpty ;
    MyListSize = Zero | One | Many ;
    PListOp = PListAnd | PListOr ;
    Verbosity = Concise | Verbose ;

    Hilight = Strong | Emph | Underline ;

  oper

    -- Keywords

    hilight : Hilight -> Str -> Str ;
    hilight emph str =
      openTag emph ++ str ++ closeTag emph ;

    openTag : Hilight -> Str = \t -> case t of {
      Strong => "<strong>" ;
      Emph => "<em>" ;
      Underling => "<u>"
    } ;

    closeTag : Hilight -> Str = \t -> case t of {
      Strong => "</strong>" ;
      Emph => "</em>" ;
      Underling => "</u>"
    } ;

    ifKw : Str = hilight Strong "if" ;
    thenKw : Str = hilight Strong "then" ;
    elseKw : Str = hilight Strong "else" ;
    andKw : Str = hilight Strong "and" ;
    orKw : Str = hilight Strong "or" ;
    allKw : Str = hilight Strong (hilight Emph "all of") ;
    anyKw : Str = hilight Strong (hilight Emph "any of") ;

    LinMetadata : Type = {s : Str ; isEmpty : IsEmpty} ;
    linRowMd : LinMetadata -> Str = \md ->
      case md.isEmpty of {
        Empty => md.s ;
        NonEmpty => "; i.e." ++ md.s
      } ;
    linTypeDeclMd : LinMetadata -> Str = \md ->
      case md.isEmpty of {
        Empty => "is a class" ++ md.s ;
        NonEmpty => "is" ++ md.s
      } ;
    linType : SS -> Str = \s -> "of type" ++ s.s ;

  lin
    -- Placeholder, or skip some constructs?
    EmptyS = {s = ""} ;
    TypeDeclS id td = {s = paragraph id.s td.s} ;
    ExprS id expr = {s = paragraph id.s expr.s} ;
    EvalS id expr = {s = paragraph id.s ("evaluate" ++ expr.s)} ;
    EvalWhetherS id expr = {s = paragraph id.s ("evaluate whether" ++ expr.s)} ;
    AssignS id name expr = {
      s = paragraph id.s (
            dl (quote name.s ++ hilight Strong "is calculated by")
               expr.s
          )
      } ;
    LetIsTrue id name expr = {
      s = paragraph id.s (
            dl (quote name.s ++ ifKw)
               expr.s
          )
       } ;
    AtomicConcept id name = {s = paragraph id.s (name.s ++ "is an atomic concept.")} ;

    -- Metadata
    MkMetadata str = str ** {isEmpty = NonEmpty} ;
    NoMetadata = {s = [] ; isEmpty = Empty} ;

    -- Type declarations
    MkRowTypeDecl md field typ = {s = "its" ++ field.s ++ linType typ ++ linRowMd md} ;
    MkRowDecl md field = {s = "its" ++ field.s ++ linRowMd md} ;

    -- These funs are automatically generated from cat [RowTypeDecl]{0} ;
    -- : [RowTypeDecl]
    BaseRowTypeDecl = {s = [] ; isEmpty = Empty} ;
    -- : RowTypeDecl -> [RowTypeDecl] -> [RowTypeDecl]
    ConsRowTypeDecl t ts = {
      s = li t.s ++ ts.s ;
      isEmpty = NonEmpty
      } ;

    MkTypeDecl md name rtds = {
      s = artIndef ++ name.s ++ linTypeDeclMd md ++
            case rtds.isEmpty of {
              Empty => [] ;
              NonEmpty => "Each" ++ name.s ++
                      "has associated with it information like" ++
                      ul rtds.s
            }
      } ;

  oper
    linebreak = "∞" ;
    hr = linebreak ++ "<hr/>" ++ linebreak ;

    quote : Str -> Str ;
    quote str = "<u>" ++ BIND ++ str ++ BIND ++ "</u>" ;

    quoteSS : SS -> SS ;
    quoteSS ss = {s = quote ss.s} ;

    paragraph = overload {
      paragraph : Str -> Str = \s -> "<p>" ++ s ++ "</p>" ;
      paragraph : (id : Str) -> Str -> Str = \i,s -> "<p id=\"" ++ BIND ++ i ++ BIND ++ "\">" ++ s ++ "</p>"
      } ;

    artIndef = pre {
      "eu" | "Eu" | "uni" | "Uni" => "A" ;
      "un" | "Un" => "An" ;
      "a" | "e" | "i" | "o" | "A" | "E" | "I" | "O" => "An" ;
      _ => "A"
      } ;

    conjXss = overload {
      conjXss : ListX0 -> SS = \xs -> ss (conjX "" xs) ;
      conjXss : Str -> ListX0 -> SS = \c,xs -> ss (conjX c xs)
    } ;

    conjX : Str -> ListX0 -> Str = \conj,xs -> case xs.size of {
      Many => conjunctX (ss conj) xs ;
      _ => xs.s2
      } ;
    linArgs = overload {
      linArgs : Str -> ListX0 -> Str = \s,xs ->
        case xs.size of {
          Zero => "" ;
          _    => s ++ conjX "and" xs } ;
      linArgs : Str -> ListX0 -> Str -> Str = \pr,xs,pst ->
        case xs.size of {
          Zero => "" ;
          _    => pr ++ conjX "and" xs ++ pst }
      } ;

    -- Bin expr
    mkBinExpr : Str -> Str -> {s : Verbosity => Str} ;
    mkBinExpr short long = {
      s = table {
        Concise => hilight Strong short ;
        Verbose => long }
      } ;

    -- ul = overload {
      ul,li : Str -> Str ;
      ul str = "<ul>" ++ str ++ "</ul>" ;
      li str = "<li>" ++ str ++ "</li>" ;

    -- }

    dl = overload {
      dl : (t,d : Str) -> Str  = \t,d ->
          "<dl>"
        ++ "<dt>" ++ t ++ "</dt>"
        ++ "<dd>" ++ d ++ "</dd>"
        ++ "</dl>"
        ;
      dl : Hilight -> (t, d : Str) -> Str = \emph,t,d ->
          "<dl>"
        ++ "<dt>" ++ hilight emph t ++ "</dt>"
        ++ "<dd>" ++ d ++ "</dd>"
        ++ "</dl>"
    } ;


    ite : (i,t,e : Str) -> Str ;
    ite i t e =
      dl ifKw i ++
      (dl thenKw t ++
       dl elseKw e) ;

  lin

    -- Expressions
    MkName str = str ;

    QuoteVar name = {s = quote name.s} ;
    Var name = name ;
    Lit name = name ;
    Unary op expr = {
      s = dl op.s expr.s
      } ;
    BinExpr op e1 e2 = {
      s = e1.s
       ++ op.s ! Concise
       ++ e2.s
      } ;

    VerboseBinExpr op e1 e2 = {
      s = dl "" e1.s
       ++ dl (op.s ! Verbose) e2.s
      } ;

    Round expr precision = expr ;
    -- { -- for being extra verbose
    --   s = dl "rounding" expr.s
    --    ++ dl "to precision of" (precision.s ++ "decimals")
    -- } ;

    Default expr default = {
      s = dl ("if" ++ expr.s ++ "is uncertain, then") default.s
       ++ dl "else" expr.s
    } ;

    IfThenElse if then else = {
      s = ite if.s then.s else.s
      } ;

    FirstIfThen i t = {
      s = dl "if" i.s
      ++ dl "then" t.s
    } ;
    MiddleIfThen i t = {
      s = dl "else if" i.s
      ++ dl "then" t.s
    } ;
    NilIfThen e = {
      s = dl "else" e.s
    } ;

    BaseIfThen = baseIfThen ;
    ConsIfThen = consIfThen ;
    Elif its = its ; -- {s = ul its.s} ;


    -- : (description : String) -> (arg : Expr) -> Expr ;
    FunApp1 adding_up entities = {
      s = dl adding_up.s entities.s
      } ;

    -- : (desc1 : String) -> (arg1 : Expr) -> (desc2 : String) -> (arg2 : Expr) -> Expr ;
    FunApp2 desc1 entities desc2 condition = {
      s = dl desc1.s entities.s
       ++ dl desc2.s condition.s
      } ;

    -- If no annotation available, just put together function and arguments in a definition list
    -- : Expr -> [Expr] -> Expr ;
    FunApp f xs = {
      s = dl f.s (linArgs "" xs)
      } ;
    -- Record : (Row Expr) -> Expr ;               -- record construction

    -- : Expr -> Name -> Expr ;             -- record projection
    Project rec field = {
      s = glue rec.s "'s" ++
          field.s
      } ;

    -- don't print out the record, only the field.
    -- Whether to choose this or Project should depend on annotations in the Lam4 program.
    OnlyFieldProject _rec field = {
      s = field.s
      } ;

    -- : Name -> Metadata -> [Name] -> Expr -> Expr ;  -- Function
    Fun funname md args body = {
      s =
      -- "Function" ++ funname.s ++ ":" ++ linebreak ++
      --  ++ linArgs "given" args ", return" ++
      body.s
      } ;

    -- : S -> Expr -> Expr ;
    Let decl expr = {
      s = decl.s ++ hr ++ expr.s
    } ;

    -- : Name -> Expr -> Expr ;
    Record name field = {s = glue name.s "'s" ++ field.s} ;
    Sig parents relations = cc2 (conjXss parents) (conjXss relations) ;
    -- Let :        Decl Expr
    -- StatementBlock :  (NonEmpty Statement)

    NormIsInfringed name = {s = name.s ++ "is infringed"} ;

    -- : (predname : Name)  Metadata -> (args : [Name]) -> Expr -> Expr ;
    Predicate is_eligible md args body = {
      s = case args.size of {
            -- single argument, "args is_eligible"
            One => conjX "" args
                ++ glue is_eligible.s "," ;
            -- multiple arguments, "is_eligible holds"
            _   => quote is_eligible.s
                ++ linArgs "holds for" args ","
          } ++ "if" ++ body.s ;
      } ;

    -- : Expr -> [Expr] -> Expr ;
    PredApp f xs = {
      s = case xs.size of {
        One => conjX "and" xs ++ "is" ++ f.s ; -- business plan is known
        _ => f.s ++ "holds for" ++ conjX "and" xs }
      } ;

    PredAppMany op preds args = {
      s = quote (conjX "and" args) ++ "is" ++ conjX (op.s ! Verbose) preds
    } ;

    Fold combine nil over = {
      s = linebreak ++
      "Combine" ++ quote over.s ++ "into one value," ++ linebreak
       ++ "using the function" ++ quote combine.s ++ "," ++ linebreak
       ++ "starting from" ++ nil.s
    } ;

    -- When generating natural language for some file that defines a bunch of stuff like cons, map, filter,
    -- apply this function instead to keep it in the AST
    -- but skip linearization.
    -- : Name -> Expr
    KnownFunction _ = {s = ""} ;

    -- Unary and binary operators
    Not = ss (hilight Strong "it is not the case that") ;
    Floor = ss "floor of" ;
    Ceiling = ss "ceiling of" ;
    IntegerToFraction = ss "" ; -- not important in NLG
    UnaryMinus = ss "-" ;

    UnaryMinusExpr expr = {s = glue "-" expr.s} ;

    Or = mkBinExpr "||" "or" ;
    And = mkBinExpr "&&" "and" ;
    Plus = mkBinExpr "+" "added to" ;
    Minus = mkBinExpr "-" "subtracted from" ;
    Modulo = mkBinExpr "%" "modulo" ;
    Mult = mkBinExpr "*" "multiplied by" ;
    Divide = mkBinExpr "/" "divided by" ;
    Lt = mkBinExpr "<" "is less than" ;
    Le = mkBinExpr "≤" "is less than or equal" ;
    Gt = mkBinExpr ">" "is greater than" ;
    Ge = mkBinExpr "≥" "is greater than or equal to" ;
    Eq = mkBinExpr "=" "equals to" ;
    Ne = mkBinExpr "≠" "is not equal to" ;


-------------------------
  -- List operations
  oper
    ListX0 : Type = ListX ** {size : MyListSize} ;

    baseListX0 : ListX0 = {
      s1, s2 = "" ;
      size = Zero
    } ;

    conslListX0 : SS -> ListX0 -> ListX0 = \x,xs -> case xs.size of {
        Many => xs ** {
          s1 = xs.s1 ++ bindComma ++ xs.s2 ;
          s2 = x.s
          } ;
        One => xs ** {
          s2 = x.s ;
          size = Many
          } ;
        Zero => xs ** {
          s1 = x.s ;
          size = One
          }
      } ;

    consrListX0 : SS -> ListX0 -> ListX0 = \x,xs -> case xs.size of {
        Many => xs ** {
          s1 = x.s ++ bindComma ++ xs.s1 ;
          } ;
        One => xs ** {
          s1 = x.s ;
          size = Many
          } ;
        Zero => xs ** {
          s2 = x.s ;
          size = One
          }
      } ;

    baseIfThen : SS -> SS -> SS = \s1,s2 -> {
      s =
          -- li
          s1.s
        ++
          -- li
          s2.s
      } ;

    consIfThen : SS -> SS -> SS = \s,ss -> ss ** {
      s =
          -- li
          s.s
        ++ ss.s
      } ;
  -- Special for flattening nested And/Or
  LinLExpr : Type = {s : PListOp => Str} ;
  LinListOp : Type = {s : Str ; op : PListOp} ;

  conjTable : PListOp => Str = table {
    PListAnd => andKw ;
    PListOr => orKw } ;

  headerTable : PListOp => Str = table {
    PListAnd => allKw ;
    PListOr => anyKw } ;

  baseLExpr : SS -> SS -> LinLExpr = \s1,s2 -> {
    s = \\conj =>
         dl (conjTable ! conj) s1.s
      ++
         dl (conjTable ! conj) s2.s
    } ;

  consLExpr : SS -> LinLExpr -> LinLExpr = \s,ss -> ss ** {
    s = \\conj =>
         dl (conjTable ! conj) s.s
      ++
         ss.s ! conj

    } ;

  conjLExpr : LinListOp -> LinLExpr -> SS = \co,ss -> {
    s = ss.s ! co.op
    } ;

lin
    BaseExpr, BaseName = baseListX0 ;
    ConsExpr, ConsName = consrListX0 ;
    ConjExpr = conjXss ;

    BaseLExpr = baseLExpr ;
    ConsLExpr = consLExpr ;
    ApplyListOp = conjLExpr ;

    ListAnd = {s = "" ; op = PListAnd} ;
    ListOr = {s = "" ; op = PListOr} ;
    coerceListExpr = id SS ;
}

{-
— <some kind of description>
LOTTERY {
  — how much can be won from the jackpot
 `total jackpot`: Integer,
   — whether buying tickets from this lottery is tax deductible
  `tax deductible status`: Boolean
}


This can be rendered as:


A/An ‘Lottery’ is <insert description>.
Each lottery has associated with it information like
  * its ‘total jackpot’; i.e., how much can be won from the jackpot
  * its ‘tax deductible status’; i.e., whether buying tickets from this lottery is tax deductible
-}