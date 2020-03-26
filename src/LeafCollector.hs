module LeafCollector where

import Language.Haskell.Exts
import Language.Haskell.Exts.Syntax

type Value = String

class Node a where
  getLeaves :: a SrcSpanInfo -> Int -> Either String [Leaf]

data Leaf =
  Leaf [Identifier]
       [Int]
       Int
       Value

instance Show Leaf where
  show (Leaf idents is len v) =
    "Identifications:" ++
    show idents ++
    "; Siblindindicies:" ++
    show is ++ "; Length:" ++ show len ++ "; Value:" ++ v

setLeafHead :: Identifier -> Leaf -> Int -> Leaf
setLeafHead ind (Leaf idents is len v) i =
  Leaf (ind : idents) (i : is) (len + 1) v

setLeafHeads :: Identifier -> [Leaf] -> Int -> [Leaf]
setLeafHeads ident ns index = map (\n -> setLeafHead ident n index) ns

setIdentHead :: Leaf -> Identifier -> Leaf
setIdentHead (Leaf idents is len v) ind = Leaf (ind : idents) is (len + 1) v

data FunctionLeaves =
  FunctionLeaves String
                 [Leaf]

instance Show FunctionLeaves where
  show (FunctionLeaves s nss) = s ++ concatMap show nss

valueOf :: Leaf -> Value
valueOf (Leaf _ _ _ v) = v

buildDecl :: Decl SrcSpanInfo -> Either String FunctionLeaves
buildDecl (FunBind _ ms) = do
  (r1, index') <- foldNodes' 0 FunBind' ms
  return (FunctionLeaves (nameOfFun (head ms)) r1)
buildDecl x = Left ("No Pattern in Decl for:" ++ show x)

nameOfFun :: Match SrcSpanInfo -> String
nameOfFun (Match _ name _ _ _) = nameOf name
nameOfFun (InfixMatch _ _ name _ _ _) = nameOf name

nameOf :: Name SrcSpanInfo -> String
nameOf (Ident _ name) = name
nameOf (Symbol _ name) = name

instance Node Match where
  getLeaves (Match _ _ ps rhs Nothing) index = do
    (r1, index') <- foldNodes 0 index Match' ps
    r2 <- getLeaves rhs index'
    return (r1 ++ setLeafHeads Match' r2 index)
  getLeaves (Match _ _ ps rhs (Just binds)) index = do
    (r1, index') <- foldNodes 0 index Match' ps
    r2 <- getLeaves rhs index'
    r3 <- getLeaves binds (index' + 1)
    return (r1 ++ setLeafHeads Match' (r2 ++ r3) index)
  getLeaves (InfixMatch _ pat _ ps rhs Nothing) index = do
    r1 <- getLeaves pat 0
    (r2, index') <- foldNodes 1 index InfixMatch' ps
    r3 <- getLeaves rhs index'
    return
      (setLeafHeads InfixMatch' r1 index ++
       r2 ++ setLeafHeads InfixMatch' r3 index)
  getLeaves (InfixMatch _ pat _ ps rhs (Just binds)) index = do
    r1 <- getLeaves pat 0
    (r2, index') <- foldNodes 1 index InfixMatch' ps
    r3 <- getLeaves rhs index'
    r4 <- getLeaves binds (index' + 1)
    return
      (setLeafHeads InfixMatch' r1 index ++
       r2 ++ setLeafHeads InfixMatch' (r3 ++ r4) index)

instance Node Decl where
  getLeaves (PatBind _ pat rhs (Just bind)) index = do
    r1 <- getLeaves pat 0
    r2 <- getLeaves rhs 1
    r3 <- getLeaves bind 2
    return (setLeafHeads PatBind' (r1 ++ r2 ++ r3) index)
  getLeaves (PatBind _ pat rhs Nothing) index = do
    r1 <- getLeaves pat 0
    r2 <- getLeaves rhs 1
    return (setLeafHeads PatBind' (r1 ++ r2) index)
  getLeaves a index = Left ("No Pattern in Decl for " ++ show a)

instance Node Pat where
  getLeaves (PVar _ name) index = do
    r1 <- getLeaves name 0
    return (setLeafHeads PVar' r1 index)
  getLeaves (PLit _ sign lit) index = do
    r1 <- getLeaves sign 0
    r2 <- getLeaves lit 1
    return (setLeafHeads PLit' (r1 ++ r2) index)
  getLeaves (PList _ pats) index = do
    (r1, _) <- foldNodes 0 index PList' pats
    return r1
  getLeaves (PParen _ pat) index = do
    r1 <- getLeaves pat 0
    return (setLeafHeads PParen' r1 index)
  getLeaves (PTuple _ _ pats) index = do
    (r1, _) <- foldNodes 0 index PTuple' pats
    return r1
  getLeaves (PWildCard _) index = return [Leaf [PWildCard'] [index] 1 "_"]
  getLeaves (PInfixApp _ pat1 qName pat2) index = do
    r1 <- getLeaves pat1 0
    r2 <- getLeaves qName 1
    r3 <- getLeaves pat2 2
    return (setLeafHeads PInfixApp' (r1 ++ r2 ++ r3) index)
  getLeaves (PApp _ qName pats) index = do
    r1 <- getLeaves qName 0
    (r2, _) <- foldNodes 1 index PApp' pats
    return (setLeafHeads PApp' r1 index ++ r2)
  getLeaves a index = Left ("No Pattern in Pat:" ++ show a)

instance Node Sign where
  getLeaves (Signless _) index = return [Leaf [Signless'] [index] 1 "+"]
  getLeaves (Negative _) index = return [Leaf [Negative'] [index] 1 "-"]

instance Node Rhs where
  getLeaves (UnGuardedRhs _ exp) index = do
    r1 <- getLeaves exp 0
    return (setLeafHeads UnGuardedRhs' r1 index)
  getLeaves (GuardedRhss _ rhss) index = do
    (r2, _) <- foldNodes 0 index GuardedRhss' rhss
    return r2

instance Node GuardedRhs where
  getLeaves (GuardedRhs _ stmts exp) index = do
    (r1, index') <- foldNodes 0 index GuardedRhs' stmts
    r2 <- getLeaves exp index'
    return (r1 ++ setLeafHeads GuardedRhs' r2 index)

instance Node Stmt where
  getLeaves (Generator _ pat exp) index = do
    r1 <- getLeaves pat 0
    r2 <- getLeaves exp 1
    return (setLeafHeads Generator' (r1 ++ r2) index)
  getLeaves (Qualifier _ exp) index = do
    r1 <- getLeaves exp 0
    return (setLeafHeads Qualifier' r1 index)
  getLeaves (RecStmt _ stmts) index = do
    (r1, _) <- foldNodes 0 index RecStmt' stmts
    return r1
  getLeaves (LetStmt _ bind) index = do
    r1 <- getLeaves bind 0
    return (setLeafHeads LetStmt' r1 index)

instance Node Exp where
  getLeaves (If _ exp1 exp2 exp3) index = do
    r1 <- getLeaves exp1 0
    r2 <- getLeaves exp2 1
    r3 <- getLeaves exp3 2
    return (setLeafHeads If' (r1 ++ r2 ++ r3) index)
  getLeaves (InfixApp _ exp1 qop exp2) index = do
    r1 <- getLeaves exp1 0
    r2 <- getLeaves qop 1
    r3 <- getLeaves exp2 2
    return (setLeafHeads InfixApp' (r1 ++ r2 ++ r3) index)
  getLeaves (Var _ qname) index = do
    r1 <- getLeaves qname 0
    return (setLeafHeads Var' r1 index)
  getLeaves (Do _ stmts) index = do
    (r1, _) <- foldNodes 0 index Do' stmts
    return r1
  getLeaves (Lit _ lit) index = do
    r1 <- getLeaves lit 0
    return (setLeafHeads Lit' r1 index)
  getLeaves (Paren _ p) index = do
    r1 <- getLeaves p 0
    return (setLeafHeads Paren' r1 index)
  getLeaves (Con _ qName) index = do
    r1 <- getLeaves qName 0
    return (setLeafHeads Con' r1 index)
  getLeaves (List _ exps) index = do
    (r, _) <- foldNodes 0 index List' exps
    return r
  getLeaves (Let _ bind exp) index = do
    r1 <- getLeaves bind 0
    r2 <- getLeaves exp 1
    return (setLeafHeads Let' (r1 ++ r2) index)
  getLeaves (Lambda _ pats exp) index = do
    (r1, index') <- foldNodes 0 index Lambda' pats
    r2 <- getLeaves exp index'
    return (r1 ++ setLeafHeads Lambda' r2 index)
  getLeaves (Case _ exp alts) index = do
    r1 <- getLeaves exp 0
    (r2, _) <- foldNodes 1 index Case' alts
    return (setLeafHeads Case' r1 index ++ r2)
  getLeaves (App _ exp1 exp2) index = do
    r1 <- getLeaves exp1 0
    r2 <- getLeaves exp2 1
    return (setLeafHeads App' (r1 ++ r2) index)
  getLeaves a index = Left ("Missing pattern in Exp:" ++ show a)

foldNodes ::
     (Node a)
  => Int
  -> Int
  -> Identifier
  -> [a SrcSpanInfo]
  -> Either String ([Leaf], Int)
foldNodes newIndex oldIndex ident xs = do
  (leaves, indexCount) <-
    foldl
      (\results x -> do
         (r, newIndex') <- results
         leaves <- getLeaves x newIndex'
         return (r ++ leaves, newIndex' + 1))
      (Right ([], newIndex))
      xs
  return (setLeafHeads ident leaves oldIndex, indexCount)

foldNodes' ::
     (Node a)
  => Int
  -> Identifier
  -> [a SrcSpanInfo]
  -> Either String ([Leaf], Int)
foldNodes' index ident xs = do
  (leaves, indexCount) <-
    foldl
      (\results x -> do
         (r, index') <- results
         leaves <- getLeaves x index'
         return (r ++ leaves, index' + 1))
      (Right ([], index))
      xs
  return (map (\n -> setIdentHead n ident) leaves, indexCount)

instance Node Binds where
  getLeaves (BDecls _ ipBinds) index = do
    (r, _) <- foldNodes 0 index BDecls' ipBinds
    return r
  getLeaves (IPBinds _ decls) index = do
    (r, _) <- foldNodes 0 index IPBinds' decls
    return r

instance Node IPName where
  getLeaves (IPDup _ s) index = return [Leaf [IPDup'] [index] 1 s]
  getLeaves (IPLin _ s) index = return [Leaf [IPLin'] [index] 1 s]

instance Node IPBind where
  getLeaves (IPBind _ ipName exp) index = do
    r1 <- getLeaves ipName 0
    r2 <- getLeaves exp 1
    return (setLeafHeads IPBind' (r1 ++ r2) index)

instance Node Alt where
  getLeaves (Alt _ pat rhs Nothing) index = do
    r1' <- getLeaves pat 0
    r2' <- getLeaves rhs 1
    return (setLeafHeads Alt' (r1' ++ r2') index)

instance Node Literal where
  getLeaves (Int _ i' s) index = return [Leaf [Int'] [index] 1 s]
  getLeaves (String _ s s') index = return [Leaf [String'] [index] 1 (show s)]
  getLeaves (Char _ c s) index = return [Leaf [Char'] [index] 1 s]
  getLeaves (Frac _ f s) index = return [Leaf [Frac'] [index] 1 s]
  getLeaves a _ = Left ("Unkown literal: " ++ show a)

instance Node QOp where
  getLeaves (QVarOp _ qname) index = do
    r1 <- getLeaves qname 0
    return (setLeafHeads QVarOp' r1 index)
  getLeaves (QConOp _ qname) index = do
    r1 <- getLeaves qname 0
    return (setLeafHeads QConOp' r1 index)

instance Node QName where
  getLeaves (UnQual _ name) index = do
    r1 <- getLeaves name 0
    return (setLeafHeads UnQual' r1 index)
  getLeaves (Special _ con) index = do
    r1 <- getLeaves con 0
    return (setLeafHeads Special' r1 index)
  getLeaves a index = Left ("UnkownError in QName: " ++ show a)

instance Node SpecialCon where
  getLeaves (ListCon _) index = return [Leaf [ListCon'] [index] 1 "[]"]
  getLeaves (Cons _) index = return [Leaf [Cons'] [index] 1 ":"]
  getLeaves (UnitCon _) index = return [Leaf [UnitCon'] [index] 1 "()"]
  getLeaves (FunCon _) index = return [Leaf [FunCon'] [index] 1 "->"]
  getLeaves (ExprHole _) index = return [Leaf [ExprHole'] [index] 1 "_"]
  getLeaves a index = Left ("UnkownError in special con: " ++ show a)

instance Node Name where
  getLeaves (Ident _ name) index = return [Leaf [Ident'] [index] 1 name]
  getLeaves (Symbol _ name) index = return [Leaf [Symbol'] [index] 1 name]

instance Show Identifier where
  show FunBind' = "FunBind"
  show Match' = "Match"
  show InfixMatch' = "MethodDecl"
  show PVar' = "PVar"
  show PLit' = "PLit"
  show PList' = "PList"
  show Signless' = "Signless"
  show Negative' = "Negative"
  show UnGuardedRhs' = "UnGuardedRhs"
  show GuardedRhss' = "GuardedRhss"
  show GuardedRhs' = "GuardedRhs"
  show Generator' = "Generator"
  show Qualifier' = "Qualifier"
  show RecStmt' = "RecStmt"
  show If' = "If"
  show InfixApp' = "InfixApp"
  show Var' = "Var"
  show BDecls' = "BDecls"
  show Lit' = "Lit"
  show Int' = "Int"
  show String' = "String"
  show Char' = "Char"
  show Frac' = "Frac"
  show QVarOp' = "QVarOp"
  show QConOp' = "QConOp"
  show UnQual' = "UnQual"
  show Ident' = "Ident"
  show Unqual' = "Unqual"
  show Symbol' = "Symbol"
  show QName' = "QName"
  show Lambda' = "Lambda"
  show Alt' = "Alt"
  show Case' = "Case"
  show App' = "App"
  show Paren' = "Paren"
  show Con' = "Con"
  show List' = "List"
  show IPDup' = "IPDup"
  show IPLin' = "IPLin"
  show IPBind' = "IPBind"
  show IPBinds' = "IPBinds"
  show Let' = "Let"
  show PatBind' = "PatBind"
  show PParen' = "PParen"
  show PApp' = "PApp"
  show PInfixApp' = "PInfixApp"
  show ListCon' = "ListCon"
  show ExprHole' = "ExprHole"
  show Special' = "Special"
  show Cons' = "Cons"
  show UnitCon' = "UnitCon"
  show FunCon' = "FunCon"
  show PTuple' = "PTuple"
  show PWildCard' = "PWildCard"
  show LetStmt' = "LetStmt"
  show Do' = "LetStmt"

data Identifier
  = FunBind'
  | Match'
  | InfixMatch'
  | PVar'
  | PLit'
  | PList'
  | Signless'
  | Negative'
  | UnGuardedRhs'
  | GuardedRhss'
  | GuardedRhs'
  | Generator'
  | Qualifier'
  | RecStmt'
  | If'
  | InfixApp'
  | Var'
  | Lit'
  | Int'
  | String'
  | Char'
  | Frac'
  | QVarOp'
  | QConOp'
  | UnQual'
  | Ident'
  | Unqual'
  | Symbol'
  | QName'
  | Lambda'
  | Alt'
  | Case'
  | App'
  | Paren'
  | Con'
  | List'
  | IPDup'
  | IPLin'
  | IPBind'
  | IPBinds'
  | Let'
  | PatBind'
  | PParen'
  | PApp'
  | PInfixApp'
  | ListCon'
  | Special'
  | ExprHole'
  | Cons'
  | UnitCon'
  | FunCon'
  | PTuple'
  | PWildCard'
  | BDecls'
  | LetStmt'
  | Do'
