{-# LANGUAGE GADTs, UndecidableInstances #-}
module Lam4.Render.Lam4Gf where

import Control.Monad.Identity (Identity ( Identity, runIdentity))
import Control.Monad (MonadPlus, ap, mzero, mplus)
import PGF (Expr, mkApp, mkCId, mkFloat, mkInt, mkStr, showCId, showExpr, unApp, unFloat, unInt, unStr )

----------------------------------------------------
-- automatic translation from GF to Haskell
----------------------------------------------------

class Gf a where
  gf :: a -> Expr
  fg :: Expr -> a

instance Gf GString where
  gf (GString x) = mkStr x
  fg t =
    case unStr t of
      Just x  ->  GString x
      Nothing -> error ("no GString " ++ show t)

instance Gf GInt where
  gf (GInt x) = mkInt x
  fg t =
    case unInt t of
      Just x  ->  GInt x
      Nothing -> error ("no GInt " ++ show t)

instance Gf GFloat where
  gf (GFloat x) = mkFloat x
  fg t =
    case unFloat t of
      Just x  ->  GFloat x
      Nothing -> error ("no GFloat " ++ show t)

----------------------------------------------------
-- below this line machine-generated
instance (Gf (Tree a)) => Show (Tree a) where
    show = showExpr [] . gf
----------------------------------------------------

type GBinOp = Tree GBinOp_
data GBinOp_
type GExpr = Tree GExpr_
data GExpr_
type GIfThen = Tree GIfThen_
data GIfThen_
type GLExpr = Tree GLExpr_
data GLExpr_
type GListExpr = Tree GListExpr_
data GListExpr_
type GListIfThen = Tree GListIfThen_
data GListIfThen_
type GListLExpr = Tree GListLExpr_
data GListLExpr_
type GListName = Tree GListName_
data GListName_
type GListOp = Tree GListOp_
data GListOp_
type GListRowTypeDecl = Tree GListRowTypeDecl_
data GListRowTypeDecl_
type GMetadata = Tree GMetadata_
data GMetadata_
type GName = Tree GName_
data GName_
type GRowTypeDecl = Tree GRowTypeDecl_
data GRowTypeDecl_
type GS = Tree GS_
data GS_
type GTypeDecl = Tree GTypeDecl_
data GTypeDecl_
type GUnaryOp = Tree GUnaryOp_
data GUnaryOp_
type GString = Tree GString_
data GString_
type GInt = Tree GInt_
data GInt_
type GFloat = Tree GFloat_
data GFloat_

data Tree :: * -> * where
  GAnd :: Tree GBinOp_
  GDivide :: Tree GBinOp_
  GEq :: Tree GBinOp_
  GGe :: Tree GBinOp_
  GGt :: Tree GBinOp_
  GLe :: Tree GBinOp_
  GLt :: Tree GBinOp_
  GMinus :: Tree GBinOp_
  GModulo :: Tree GBinOp_
  GMult :: Tree GBinOp_
  GNe :: Tree GBinOp_
  GOr :: Tree GBinOp_
  GPlus :: Tree GBinOp_
  GApplyListOp :: GListOp -> GListLExpr -> Tree GExpr_
  GBinExpr :: GBinOp -> GExpr -> GExpr -> Tree GExpr_
  GConjExpr :: GListExpr -> Tree GExpr_
  GDefault :: GExpr -> GExpr -> Tree GExpr_
  GElif :: GListIfThen -> Tree GExpr_
  GFold :: GExpr -> GExpr -> GExpr -> Tree GExpr_
  GFun :: GName -> GMetadata -> GListName -> GExpr -> Tree GExpr_
  GFunApp :: GExpr -> GListExpr -> Tree GExpr_
  GFunApp1 :: GString -> GExpr -> Tree GExpr_
  GFunApp2 :: GString -> GExpr -> GString -> GExpr -> Tree GExpr_
  GIfThenElse :: GExpr -> GExpr -> GExpr -> Tree GExpr_
  GKnownFunction :: GName -> Tree GExpr_
  GLet :: GS -> GExpr -> Tree GExpr_
  GLit :: GName -> Tree GExpr_
  GNormIsInfringed :: GName -> Tree GExpr_
  GOnlyFieldProject :: GExpr -> GName -> Tree GExpr_
  GPredApp :: GExpr -> GListExpr -> Tree GExpr_
  GPredAppMany :: GBinOp -> GListExpr -> GListExpr -> Tree GExpr_
  GPredicate :: GName -> GMetadata -> GListName -> GExpr -> Tree GExpr_
  GProject :: GExpr -> GName -> Tree GExpr_
  GQuoteVar :: GName -> Tree GExpr_
  GRecord :: GName -> GExpr -> Tree GExpr_
  GRound :: GExpr -> GExpr -> Tree GExpr_
  GSig :: GListName -> GListExpr -> Tree GExpr_
  GUnary :: GUnaryOp -> GExpr -> Tree GExpr_
  GUnaryMinusExpr :: GExpr -> Tree GExpr_
  GVar :: GName -> Tree GExpr_
  GVerboseBinExpr :: GBinOp -> GExpr -> GExpr -> Tree GExpr_
  GFirstIfThen :: GExpr -> GExpr -> Tree GIfThen_
  GMiddleIfThen :: GExpr -> GExpr -> Tree GIfThen_
  GNilIfThen :: GExpr -> Tree GIfThen_
  GcoerceListExpr :: GExpr -> Tree GLExpr_
  GListExpr :: [GExpr] -> Tree GListExpr_
  GListIfThen :: [GIfThen] -> Tree GListIfThen_
  GListLExpr :: [GLExpr] -> Tree GListLExpr_
  GListName :: [GName] -> Tree GListName_
  GListAnd :: Tree GListOp_
  GListOr :: Tree GListOp_
  GListRowTypeDecl :: [GRowTypeDecl] -> Tree GListRowTypeDecl_
  GMkMetadata :: GString -> Tree GMetadata_
  GNoMetadata :: Tree GMetadata_
  GMkName :: GString -> Tree GName_
  GMkRowDecl :: GMetadata -> GName -> Tree GRowTypeDecl_
  GMkRowTypeDecl :: GMetadata -> GName -> GName -> Tree GRowTypeDecl_
  GAssignS :: GString -> GName -> GExpr -> Tree GS_
  GAtomicConcept :: GString -> GName -> Tree GS_
  GEmptyS :: Tree GS_
  GEvalS :: GString -> GExpr -> Tree GS_
  GEvalWhetherS :: GString -> GExpr -> Tree GS_
  GExprS :: GString -> GExpr -> Tree GS_
  GLetIsTrue :: GString -> GName -> GExpr -> Tree GS_
  GTypeDeclS :: GString -> GTypeDecl -> Tree GS_
  GMkTypeDecl :: GMetadata -> GName -> GListRowTypeDecl -> Tree GTypeDecl_
  GCeiling :: Tree GUnaryOp_
  GFloor :: Tree GUnaryOp_
  GIntegerToFraction :: Tree GUnaryOp_
  GNot :: Tree GUnaryOp_
  GUnaryMinus :: Tree GUnaryOp_
  GString :: String -> Tree GString_
  GInt :: Int -> Tree GInt_
  GFloat :: Double -> Tree GFloat_

instance Eq (Tree a) where
  i == j = case (i,j) of
    (GAnd,GAnd) -> and [ ]
    (GDivide,GDivide) -> and [ ]
    (GEq,GEq) -> and [ ]
    (GGe,GGe) -> and [ ]
    (GGt,GGt) -> and [ ]
    (GLe,GLe) -> and [ ]
    (GLt,GLt) -> and [ ]
    (GMinus,GMinus) -> and [ ]
    (GModulo,GModulo) -> and [ ]
    (GMult,GMult) -> and [ ]
    (GNe,GNe) -> and [ ]
    (GOr,GOr) -> and [ ]
    (GPlus,GPlus) -> and [ ]
    (GApplyListOp x1 x2,GApplyListOp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GBinExpr x1 x2 x3,GBinExpr y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GConjExpr x1,GConjExpr y1) -> and [ x1 == y1 ]
    (GDefault x1 x2,GDefault y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GElif x1,GElif y1) -> and [ x1 == y1 ]
    (GFold x1 x2 x3,GFold y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GFun x1 x2 x3 x4,GFun y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (GFunApp x1 x2,GFunApp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GFunApp1 x1 x2,GFunApp1 y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GFunApp2 x1 x2 x3 x4,GFunApp2 y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (GIfThenElse x1 x2 x3,GIfThenElse y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GKnownFunction x1,GKnownFunction y1) -> and [ x1 == y1 ]
    (GLet x1 x2,GLet y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GLit x1,GLit y1) -> and [ x1 == y1 ]
    (GNormIsInfringed x1,GNormIsInfringed y1) -> and [ x1 == y1 ]
    (GOnlyFieldProject x1 x2,GOnlyFieldProject y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GPredApp x1 x2,GPredApp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GPredAppMany x1 x2 x3,GPredAppMany y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GPredicate x1 x2 x3 x4,GPredicate y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (GProject x1 x2,GProject y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GQuoteVar x1,GQuoteVar y1) -> and [ x1 == y1 ]
    (GRecord x1 x2,GRecord y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GRound x1 x2,GRound y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GSig x1 x2,GSig y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GUnary x1 x2,GUnary y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GUnaryMinusExpr x1,GUnaryMinusExpr y1) -> and [ x1 == y1 ]
    (GVar x1,GVar y1) -> and [ x1 == y1 ]
    (GVerboseBinExpr x1 x2 x3,GVerboseBinExpr y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GFirstIfThen x1 x2,GFirstIfThen y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GMiddleIfThen x1 x2,GMiddleIfThen y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GNilIfThen x1,GNilIfThen y1) -> and [ x1 == y1 ]
    (GcoerceListExpr x1,GcoerceListExpr y1) -> and [ x1 == y1 ]
    (GListExpr x1,GListExpr y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListIfThen x1,GListIfThen y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListLExpr x1,GListLExpr y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListName x1,GListName y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListAnd,GListAnd) -> and [ ]
    (GListOr,GListOr) -> and [ ]
    (GListRowTypeDecl x1,GListRowTypeDecl y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GMkMetadata x1,GMkMetadata y1) -> and [ x1 == y1 ]
    (GNoMetadata,GNoMetadata) -> and [ ]
    (GMkName x1,GMkName y1) -> and [ x1 == y1 ]
    (GMkRowDecl x1 x2,GMkRowDecl y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GMkRowTypeDecl x1 x2 x3,GMkRowTypeDecl y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GAssignS x1 x2 x3,GAssignS y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GAtomicConcept x1 x2,GAtomicConcept y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GEmptyS,GEmptyS) -> and [ ]
    (GEvalS x1 x2,GEvalS y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GEvalWhetherS x1 x2,GEvalWhetherS y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GExprS x1 x2,GExprS y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GLetIsTrue x1 x2 x3,GLetIsTrue y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GTypeDeclS x1 x2,GTypeDeclS y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GMkTypeDecl x1 x2 x3,GMkTypeDecl y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GCeiling,GCeiling) -> and [ ]
    (GFloor,GFloor) -> and [ ]
    (GIntegerToFraction,GIntegerToFraction) -> and [ ]
    (GNot,GNot) -> and [ ]
    (GUnaryMinus,GUnaryMinus) -> and [ ]
    (GString x, GString y) -> x == y
    (GInt x, GInt y) -> x == y
    (GFloat x, GFloat y) -> x == y
    _ -> False

instance Gf GBinOp where
  gf GAnd = mkApp (mkCId "And") []
  gf GDivide = mkApp (mkCId "Divide") []
  gf GEq = mkApp (mkCId "Eq") []
  gf GGe = mkApp (mkCId "Ge") []
  gf GGt = mkApp (mkCId "Gt") []
  gf GLe = mkApp (mkCId "Le") []
  gf GLt = mkApp (mkCId "Lt") []
  gf GMinus = mkApp (mkCId "Minus") []
  gf GModulo = mkApp (mkCId "Modulo") []
  gf GMult = mkApp (mkCId "Mult") []
  gf GNe = mkApp (mkCId "Ne") []
  gf GOr = mkApp (mkCId "Or") []
  gf GPlus = mkApp (mkCId "Plus") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "And" -> GAnd 
      Just (i,[]) | i == mkCId "Divide" -> GDivide 
      Just (i,[]) | i == mkCId "Eq" -> GEq 
      Just (i,[]) | i == mkCId "Ge" -> GGe 
      Just (i,[]) | i == mkCId "Gt" -> GGt 
      Just (i,[]) | i == mkCId "Le" -> GLe 
      Just (i,[]) | i == mkCId "Lt" -> GLt 
      Just (i,[]) | i == mkCId "Minus" -> GMinus 
      Just (i,[]) | i == mkCId "Modulo" -> GModulo 
      Just (i,[]) | i == mkCId "Mult" -> GMult 
      Just (i,[]) | i == mkCId "Ne" -> GNe 
      Just (i,[]) | i == mkCId "Or" -> GOr 
      Just (i,[]) | i == mkCId "Plus" -> GPlus 


      _ -> error ("no BinOp " ++ show t)

instance Gf GExpr where
  gf (GApplyListOp x1 x2) = mkApp (mkCId "ApplyListOp") [gf x1, gf x2]
  gf (GBinExpr x1 x2 x3) = mkApp (mkCId "BinExpr") [gf x1, gf x2, gf x3]
  gf (GConjExpr x1) = mkApp (mkCId "ConjExpr") [gf x1]
  gf (GDefault x1 x2) = mkApp (mkCId "Default") [gf x1, gf x2]
  gf (GElif x1) = mkApp (mkCId "Elif") [gf x1]
  gf (GFold x1 x2 x3) = mkApp (mkCId "Fold") [gf x1, gf x2, gf x3]
  gf (GFun x1 x2 x3 x4) = mkApp (mkCId "Fun") [gf x1, gf x2, gf x3, gf x4]
  gf (GFunApp x1 x2) = mkApp (mkCId "FunApp") [gf x1, gf x2]
  gf (GFunApp1 x1 x2) = mkApp (mkCId "FunApp1") [gf x1, gf x2]
  gf (GFunApp2 x1 x2 x3 x4) = mkApp (mkCId "FunApp2") [gf x1, gf x2, gf x3, gf x4]
  gf (GIfThenElse x1 x2 x3) = mkApp (mkCId "IfThenElse") [gf x1, gf x2, gf x3]
  gf (GKnownFunction x1) = mkApp (mkCId "KnownFunction") [gf x1]
  gf (GLet x1 x2) = mkApp (mkCId "Let") [gf x1, gf x2]
  gf (GLit x1) = mkApp (mkCId "Lit") [gf x1]
  gf (GNormIsInfringed x1) = mkApp (mkCId "NormIsInfringed") [gf x1]
  gf (GOnlyFieldProject x1 x2) = mkApp (mkCId "OnlyFieldProject") [gf x1, gf x2]
  gf (GPredApp x1 x2) = mkApp (mkCId "PredApp") [gf x1, gf x2]
  gf (GPredAppMany x1 x2 x3) = mkApp (mkCId "PredAppMany") [gf x1, gf x2, gf x3]
  gf (GPredicate x1 x2 x3 x4) = mkApp (mkCId "Predicate") [gf x1, gf x2, gf x3, gf x4]
  gf (GProject x1 x2) = mkApp (mkCId "Project") [gf x1, gf x2]
  gf (GQuoteVar x1) = mkApp (mkCId "QuoteVar") [gf x1]
  gf (GRecord x1 x2) = mkApp (mkCId "Record") [gf x1, gf x2]
  gf (GRound x1 x2) = mkApp (mkCId "Round") [gf x1, gf x2]
  gf (GSig x1 x2) = mkApp (mkCId "Sig") [gf x1, gf x2]
  gf (GUnary x1 x2) = mkApp (mkCId "Unary") [gf x1, gf x2]
  gf (GUnaryMinusExpr x1) = mkApp (mkCId "UnaryMinusExpr") [gf x1]
  gf (GVar x1) = mkApp (mkCId "Var") [gf x1]
  gf (GVerboseBinExpr x1 x2 x3) = mkApp (mkCId "VerboseBinExpr") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "ApplyListOp" -> GApplyListOp (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "BinExpr" -> GBinExpr (fg x1) (fg x2) (fg x3)
      Just (i,[x1]) | i == mkCId "ConjExpr" -> GConjExpr (fg x1)
      Just (i,[x1,x2]) | i == mkCId "Default" -> GDefault (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "Elif" -> GElif (fg x1)
      Just (i,[x1,x2,x3]) | i == mkCId "Fold" -> GFold (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "Fun" -> GFun (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2]) | i == mkCId "FunApp" -> GFunApp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "FunApp1" -> GFunApp1 (fg x1) (fg x2)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "FunApp2" -> GFunApp2 (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3]) | i == mkCId "IfThenElse" -> GIfThenElse (fg x1) (fg x2) (fg x3)
      Just (i,[x1]) | i == mkCId "KnownFunction" -> GKnownFunction (fg x1)
      Just (i,[x1,x2]) | i == mkCId "Let" -> GLet (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "Lit" -> GLit (fg x1)
      Just (i,[x1]) | i == mkCId "NormIsInfringed" -> GNormIsInfringed (fg x1)
      Just (i,[x1,x2]) | i == mkCId "OnlyFieldProject" -> GOnlyFieldProject (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "PredApp" -> GPredApp (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "PredAppMany" -> GPredAppMany (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "Predicate" -> GPredicate (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2]) | i == mkCId "Project" -> GProject (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "QuoteVar" -> GQuoteVar (fg x1)
      Just (i,[x1,x2]) | i == mkCId "Record" -> GRecord (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "Round" -> GRound (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "Sig" -> GSig (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "Unary" -> GUnary (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "UnaryMinusExpr" -> GUnaryMinusExpr (fg x1)
      Just (i,[x1]) | i == mkCId "Var" -> GVar (fg x1)
      Just (i,[x1,x2,x3]) | i == mkCId "VerboseBinExpr" -> GVerboseBinExpr (fg x1) (fg x2) (fg x3)


      _ -> error ("no Expr " ++ show t)

instance Gf GIfThen where
  gf (GFirstIfThen x1 x2) = mkApp (mkCId "FirstIfThen") [gf x1, gf x2]
  gf (GMiddleIfThen x1 x2) = mkApp (mkCId "MiddleIfThen") [gf x1, gf x2]
  gf (GNilIfThen x1) = mkApp (mkCId "NilIfThen") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "FirstIfThen" -> GFirstIfThen (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "MiddleIfThen" -> GMiddleIfThen (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "NilIfThen" -> GNilIfThen (fg x1)


      _ -> error ("no IfThen " ++ show t)

instance Gf GLExpr where
  gf (GcoerceListExpr x1) = mkApp (mkCId "coerceListExpr") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "coerceListExpr" -> GcoerceListExpr (fg x1)


      _ -> error ("no LExpr " ++ show t)

instance Gf GListExpr where
  gf (GListExpr []) = mkApp (mkCId "BaseExpr") []
  gf (GListExpr (x:xs)) = mkApp (mkCId "ConsExpr") [gf x, gf (GListExpr xs)]
  fg t =
    GListExpr (fgs t) where
     fgs t = case unApp t of
      Just (i,[]) | i == mkCId "BaseExpr" -> []
      Just (i,[x1,x2]) | i == mkCId "ConsExpr" -> fg x1 : fgs x2


      _ -> error ("no ListExpr " ++ show t)

instance Gf GListIfThen where
  gf (GListIfThen [x1,x2]) = mkApp (mkCId "BaseIfThen") [gf x1, gf x2]
  gf (GListIfThen (x:xs)) = mkApp (mkCId "ConsIfThen") [gf x, gf (GListIfThen xs)]
  fg t =
    GListIfThen (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseIfThen" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsIfThen" -> fg x1 : fgs x2


      _ -> error ("no ListIfThen " ++ show t)

instance Gf GListLExpr where
  gf (GListLExpr [x1,x2]) = mkApp (mkCId "BaseLExpr") [gf x1, gf x2]
  gf (GListLExpr (x:xs)) = mkApp (mkCId "ConsLExpr") [gf x, gf (GListLExpr xs)]
  fg t =
    GListLExpr (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseLExpr" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsLExpr" -> fg x1 : fgs x2


      _ -> error ("no ListLExpr " ++ show t)

instance Gf GListName where
  gf (GListName []) = mkApp (mkCId "BaseName") []
  gf (GListName (x:xs)) = mkApp (mkCId "ConsName") [gf x, gf (GListName xs)]
  fg t =
    GListName (fgs t) where
     fgs t = case unApp t of
      Just (i,[]) | i == mkCId "BaseName" -> []
      Just (i,[x1,x2]) | i == mkCId "ConsName" -> fg x1 : fgs x2


      _ -> error ("no ListName " ++ show t)

instance Gf GListOp where
  gf GListAnd = mkApp (mkCId "ListAnd") []
  gf GListOr = mkApp (mkCId "ListOr") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "ListAnd" -> GListAnd 
      Just (i,[]) | i == mkCId "ListOr" -> GListOr 


      _ -> error ("no ListOp " ++ show t)

instance Gf GListRowTypeDecl where
  gf (GListRowTypeDecl []) = mkApp (mkCId "BaseRowTypeDecl") []
  gf (GListRowTypeDecl (x:xs)) = mkApp (mkCId "ConsRowTypeDecl") [gf x, gf (GListRowTypeDecl xs)]
  fg t =
    GListRowTypeDecl (fgs t) where
     fgs t = case unApp t of
      Just (i,[]) | i == mkCId "BaseRowTypeDecl" -> []
      Just (i,[x1,x2]) | i == mkCId "ConsRowTypeDecl" -> fg x1 : fgs x2


      _ -> error ("no ListRowTypeDecl " ++ show t)

instance Gf GMetadata where
  gf (GMkMetadata x1) = mkApp (mkCId "MkMetadata") [gf x1]
  gf GNoMetadata = mkApp (mkCId "NoMetadata") []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "MkMetadata" -> GMkMetadata (fg x1)
      Just (i,[]) | i == mkCId "NoMetadata" -> GNoMetadata 


      _ -> error ("no Metadata " ++ show t)

instance Gf GName where
  gf (GMkName x1) = mkApp (mkCId "MkName") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "MkName" -> GMkName (fg x1)


      _ -> error ("no Name " ++ show t)

instance Gf GRowTypeDecl where
  gf (GMkRowDecl x1 x2) = mkApp (mkCId "MkRowDecl") [gf x1, gf x2]
  gf (GMkRowTypeDecl x1 x2 x3) = mkApp (mkCId "MkRowTypeDecl") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "MkRowDecl" -> GMkRowDecl (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "MkRowTypeDecl" -> GMkRowTypeDecl (fg x1) (fg x2) (fg x3)


      _ -> error ("no RowTypeDecl " ++ show t)

instance Gf GS where
  gf (GAssignS x1 x2 x3) = mkApp (mkCId "AssignS") [gf x1, gf x2, gf x3]
  gf (GAtomicConcept x1 x2) = mkApp (mkCId "AtomicConcept") [gf x1, gf x2]
  gf GEmptyS = mkApp (mkCId "EmptyS") []
  gf (GEvalS x1 x2) = mkApp (mkCId "EvalS") [gf x1, gf x2]
  gf (GEvalWhetherS x1 x2) = mkApp (mkCId "EvalWhetherS") [gf x1, gf x2]
  gf (GExprS x1 x2) = mkApp (mkCId "ExprS") [gf x1, gf x2]
  gf (GLetIsTrue x1 x2 x3) = mkApp (mkCId "LetIsTrue") [gf x1, gf x2, gf x3]
  gf (GTypeDeclS x1 x2) = mkApp (mkCId "TypeDeclS") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "AssignS" -> GAssignS (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "AtomicConcept" -> GAtomicConcept (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "EmptyS" -> GEmptyS 
      Just (i,[x1,x2]) | i == mkCId "EvalS" -> GEvalS (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "EvalWhetherS" -> GEvalWhetherS (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ExprS" -> GExprS (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "LetIsTrue" -> GLetIsTrue (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "TypeDeclS" -> GTypeDeclS (fg x1) (fg x2)


      _ -> error ("no S " ++ show t)

instance Gf GTypeDecl where
  gf (GMkTypeDecl x1 x2 x3) = mkApp (mkCId "MkTypeDecl") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "MkTypeDecl" -> GMkTypeDecl (fg x1) (fg x2) (fg x3)


      _ -> error ("no TypeDecl " ++ show t)

instance Gf GUnaryOp where
  gf GCeiling = mkApp (mkCId "Ceiling") []
  gf GFloor = mkApp (mkCId "Floor") []
  gf GIntegerToFraction = mkApp (mkCId "IntegerToFraction") []
  gf GNot = mkApp (mkCId "Not") []
  gf GUnaryMinus = mkApp (mkCId "UnaryMinus") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Ceiling" -> GCeiling 
      Just (i,[]) | i == mkCId "Floor" -> GFloor 
      Just (i,[]) | i == mkCId "IntegerToFraction" -> GIntegerToFraction 
      Just (i,[]) | i == mkCId "Not" -> GNot 
      Just (i,[]) | i == mkCId "UnaryMinus" -> GUnaryMinus 


      _ -> error ("no UnaryOp " ++ show t)


instance Compos Tree where
  compos r a f t = case t of
    GApplyListOp x1 x2 -> r GApplyListOp `a` f x1 `a` f x2
    GBinExpr x1 x2 x3 -> r GBinExpr `a` f x1 `a` f x2 `a` f x3
    GConjExpr x1 -> r GConjExpr `a` f x1
    GDefault x1 x2 -> r GDefault `a` f x1 `a` f x2
    GElif x1 -> r GElif `a` f x1
    GFold x1 x2 x3 -> r GFold `a` f x1 `a` f x2 `a` f x3
    GFun x1 x2 x3 x4 -> r GFun `a` f x1 `a` f x2 `a` f x3 `a` f x4
    GFunApp x1 x2 -> r GFunApp `a` f x1 `a` f x2
    GFunApp1 x1 x2 -> r GFunApp1 `a` f x1 `a` f x2
    GFunApp2 x1 x2 x3 x4 -> r GFunApp2 `a` f x1 `a` f x2 `a` f x3 `a` f x4
    GIfThenElse x1 x2 x3 -> r GIfThenElse `a` f x1 `a` f x2 `a` f x3
    GKnownFunction x1 -> r GKnownFunction `a` f x1
    GLet x1 x2 -> r GLet `a` f x1 `a` f x2
    GLit x1 -> r GLit `a` f x1
    GNormIsInfringed x1 -> r GNormIsInfringed `a` f x1
    GOnlyFieldProject x1 x2 -> r GOnlyFieldProject `a` f x1 `a` f x2
    GPredApp x1 x2 -> r GPredApp `a` f x1 `a` f x2
    GPredAppMany x1 x2 x3 -> r GPredAppMany `a` f x1 `a` f x2 `a` f x3
    GPredicate x1 x2 x3 x4 -> r GPredicate `a` f x1 `a` f x2 `a` f x3 `a` f x4
    GProject x1 x2 -> r GProject `a` f x1 `a` f x2
    GQuoteVar x1 -> r GQuoteVar `a` f x1
    GRecord x1 x2 -> r GRecord `a` f x1 `a` f x2
    GRound x1 x2 -> r GRound `a` f x1 `a` f x2
    GSig x1 x2 -> r GSig `a` f x1 `a` f x2
    GUnary x1 x2 -> r GUnary `a` f x1 `a` f x2
    GUnaryMinusExpr x1 -> r GUnaryMinusExpr `a` f x1
    GVar x1 -> r GVar `a` f x1
    GVerboseBinExpr x1 x2 x3 -> r GVerboseBinExpr `a` f x1 `a` f x2 `a` f x3
    GFirstIfThen x1 x2 -> r GFirstIfThen `a` f x1 `a` f x2
    GMiddleIfThen x1 x2 -> r GMiddleIfThen `a` f x1 `a` f x2
    GNilIfThen x1 -> r GNilIfThen `a` f x1
    GcoerceListExpr x1 -> r GcoerceListExpr `a` f x1
    GMkMetadata x1 -> r GMkMetadata `a` f x1
    GMkName x1 -> r GMkName `a` f x1
    GMkRowDecl x1 x2 -> r GMkRowDecl `a` f x1 `a` f x2
    GMkRowTypeDecl x1 x2 x3 -> r GMkRowTypeDecl `a` f x1 `a` f x2 `a` f x3
    GAssignS x1 x2 x3 -> r GAssignS `a` f x1 `a` f x2 `a` f x3
    GAtomicConcept x1 x2 -> r GAtomicConcept `a` f x1 `a` f x2
    GEvalS x1 x2 -> r GEvalS `a` f x1 `a` f x2
    GEvalWhetherS x1 x2 -> r GEvalWhetherS `a` f x1 `a` f x2
    GExprS x1 x2 -> r GExprS `a` f x1 `a` f x2
    GLetIsTrue x1 x2 x3 -> r GLetIsTrue `a` f x1 `a` f x2 `a` f x3
    GTypeDeclS x1 x2 -> r GTypeDeclS `a` f x1 `a` f x2
    GMkTypeDecl x1 x2 x3 -> r GMkTypeDecl `a` f x1 `a` f x2 `a` f x3
    GListExpr x1 -> r GListExpr `a` foldr (a . a (r (:)) . f) (r []) x1
    GListIfThen x1 -> r GListIfThen `a` foldr (a . a (r (:)) . f) (r []) x1
    GListLExpr x1 -> r GListLExpr `a` foldr (a . a (r (:)) . f) (r []) x1
    GListName x1 -> r GListName `a` foldr (a . a (r (:)) . f) (r []) x1
    GListRowTypeDecl x1 -> r GListRowTypeDecl `a` foldr (a . a (r (:)) . f) (r []) x1
    _ -> r t

class Compos t where
  compos :: (forall a. a -> m a) -> (forall a b. m (a -> b) -> m a -> m b)
         -> (forall a. t a -> m (t a)) -> t c -> m (t c)

composOp :: Compos t => (forall a. t a -> t a) -> t c -> t c
composOp f = runIdentity . composOpM (Identity . f)

composOpM :: (Compos t, Monad m) => (forall a. t a -> m (t a)) -> t c -> m (t c)
composOpM = compos return ap

composOpM_ :: (Compos t, Monad m) => (forall a. t a -> m ()) -> t c -> m ()
composOpM_ = composOpFold (return ()) (>>)

composOpMonoid :: (Compos t, Monoid m) => (forall a. t a -> m) -> t c -> m
composOpMonoid = composOpFold mempty mappend

composOpMPlus :: (Compos t, MonadPlus m) => (forall a. t a -> m b) -> t c -> m b
composOpMPlus = composOpFold mzero mplus

composOpFold :: Compos t => b -> (b -> b -> b) -> (forall a. t a -> b) -> t c -> b
composOpFold z c f = unC . compos (\_ -> C z) (\(C x) (C y) -> C (c x y)) (C . f)

newtype C b a = C { unC :: b }
