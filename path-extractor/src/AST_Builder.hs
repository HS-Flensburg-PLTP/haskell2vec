module AST_Builder where
import           Data.Either
import           Language.Haskell.Exts
import           Language.Haskell.Exts.Syntax

type NodeBuilder a = [Int] -> [Identifier] -> Int -> a SrcSpanInfo -> Either String [Node]

type Value = String

class Builder a where
  build :: a SrcSpanInfo -> Int -> Either String [Node]

data Node =
    Leaf [Identifier] [Int] Int Value

instance Show Node where
  show (Leaf idents is len v) = "Identifications:" ++ show idents ++ "; Siblindindicies:" ++ show is ++ "; Length:" ++ show len ++ "; Value:" ++ v

setLeafHead :: Identifier -> Node -> Int -> Node
setLeafHead ind (Leaf idents is len v) i = Leaf (ind : idents) (i : is) (len + 1) v

setLeafHeads :: Identifier -> [Node] -> Int -> [Node]
setLeafHeads ident ns index = map (\n -> setLeafHead ident n index) ns

setIndentHead :: Node -> Identifier -> Node
setIndentHead (Leaf idents is len v) ind = Leaf (ind : idents) is (len + 1) v

data FunctionNodes =
  FunctionNodes String [[Node]]

instance Show FunctionNodes where
  show (FunctionNodes s nss) = s ++ concatMap show nss

valueOf :: Node -> Value
valueOf (Leaf _ _ _ v) = v

buildDecl :: Decl SrcSpanInfo -> Maybe FunctionNodes
buildDecl (FunBind _ ms) = return (FunctionNodes (nameOfFun (head ms)) (rights (map buildMethod ms)))
buildDecl _              = Nothing

getBuildErrors :: Decl SrcSpanInfo -> Maybe [String]
getBuildErrors (FunBind _ ms) = Just [concat (lefts (map buildMethod ms))]
getBuildErrors a              = Nothing

nameOfFun :: Match SrcSpanInfo -> String
nameOfFun (Match _ name _ _ _)        = nameOf name
nameOfFun (InfixMatch _ _ name _ _ _) = nameOf name

nameOf :: Name a -> String
nameOf (Ident _ name)  = name
nameOf (Symbol _ name) = name

buildMethod :: Match SrcSpanInfo -> Either String [Node]
buildMethod (Match _ _ ps rhs Nothing) =
    do
      (r1, index') <- foldBuilder' 0 Match' ps
      r2 <- build rhs index'
      return (r1 ++ map (\n -> setIndentHead n Match') r2)
buildMethod (Match _ _ ps rhs (Just binds)) =
    do
      (r1, index') <- foldBuilder' 0 Match' ps
      r2 <- build rhs index'
      r3 <- build binds (index' + 1)
      return (r1 ++ map (\n -> setIndentHead n Match') (r2 ++ r3))
buildMethod (InfixMatch _ pat _ ps rhs Nothing) =
  do
    r1 <- build pat 0
    (r2, index') <- foldBuilder' 1 InfixMatch' ps
    r3 <- build rhs index'
    return (
      map (\n -> setIndentHead n InfixMatch') r1 ++
      r2 ++
      map (\n -> setIndentHead n InfixMatch') r3)
buildMethod (InfixMatch _ pat _ ps rhs (Just binds)) =
  do
    r1 <- build pat 0
    (r2, index') <- foldBuilder' 1 InfixMatch' ps
    r3 <- build rhs index'
    r4 <- build binds (index' + 1)
    return (
      map (\n -> setIndentHead n InfixMatch') r1 ++
      r2 ++
      map (\n -> setIndentHead n InfixMatch') (r3 ++ r4))

instance Builder Decl where
  build (PatBind _ pat rhs (Just bind)) index =
    do
      r1 <- build pat 0
      r2 <- build rhs 1
      r3 <- build bind 2
      return (setLeafHeads PatBind' (r1 ++ r2 ++ r3) index)
  build (PatBind _ pat rhs Nothing) index =
    do
      r1 <- build pat 0
      r2 <- build rhs 1
      return (setLeafHeads PatBind' (r1 ++ r2) index)
  build a index = Left ("No Pattern in Decl for " ++ show a)


instance Builder Pat where
  build (PVar _ name) index =
    do
      r1 <- build name 0
      return (setLeafHeads PVar' r1 index)
  build (PLit _ sign lit) index =
    do
      r1 <- build sign 0
      r2 <- build lit 1
      return (setLeafHeads PLit' (r1 ++ r2) index)
  build (PList _ pats) index =
    do
      (r1,_) <- foldBuilder 0 index PList' pats
      return r1
  build (PParen _ pat) index =
    do
      r1 <- build pat 0
      return (setLeafHeads PParen' r1 index)
  build (PTuple _ _ pats) index =
    do
      (r1,_) <- foldBuilder 0 index PTuple' pats
      return r1
  build (PWildCard _) index = return [Leaf [PWildCard'] [index] 0 "_"]
  build (PInfixApp _ pat1 qName pat2) index =
    do
      r1 <- build pat1 0
      r2 <- build qName 1
      r3 <- build pat2 2
      return (setLeafHeads PInfixApp' (r1 ++ r2 ++ r3) index)
  build (PApp _ qName pats) index =
    do
      r1 <- build qName 0
      (r2, _) <- foldBuilder 1 index PApp' pats
      return (setLeafHeads PApp' r1 index ++ r2)
  build a index = Left ("Error in Pat:" ++ show a)

--buildNode parent i id p = (Leaf (show p) parent id, id+1)
--

instance Builder Sign where
  build (Signless _) index = return [Leaf [Signless'] [index] 0 "+"]
  build (Negative _) index = return [Leaf [Negative'] [index] 0 "-"]

--buildPVar :: NodeBuilder PVar
--buildPVar

instance Builder Rhs where
  build (UnGuardedRhs _ exp) index =
    do
      r1 <- build exp 0
      return (setLeafHeads UnGuardedRhs' r1 index)
  build (GuardedRhss _ rhss) index =
    do
      (r2, _) <- foldBuilder 0 index GuardedRhss' rhss
      return r2

instance Builder GuardedRhs where
  build (GuardedRhs _ stmts exp) index =
    do
      (r1, index') <- foldBuilder 0 index GuardedRhs' stmts
      r2 <- build exp index'
      return (r1 ++ setLeafHeads GuardedRhs' r2 index)

instance Builder Stmt where
  build (Generator _ pat exp) index =
    do
      r1 <- build pat 0
      r2 <- build exp 1
      return (setLeafHeads Generator' (r1 ++ r2) index)
  build (Qualifier _  exp) index =
    do
      r1 <- build exp 0
      return (setLeafHeads Qualifier' r1 index)
  build (RecStmt _ stmts) index =
    do
      (r1,_) <- foldBuilder 0 index RecStmt' stmts
      return r1
  build (LetStmt _ bind) index =
    do
      r1 <- build bind 0
      return (setLeafHeads LetStmt' r1 index)

instance Builder Exp where
  build (If _ exp1 exp2 exp3) index =
    do
      r1 <- build exp1 0
      r2 <- build exp2 1
      r3 <- build exp3 2
      return (setLeafHeads If' (r1 ++ r2 ++ r3) index)
  build (InfixApp _ exp1 qop exp2) index =
    do
      r1 <- build exp1 0
      r2 <- build qop 1
      r3 <- build exp2 2
      return (setLeafHeads InfixApp' (r1 ++ r2 ++ r3) index)
  build (Var _ qname) index =
    do
      r1 <- build qname 0
      return (setLeafHeads Var' r1 index)
  build (Do _ stmts) index =
    do
      (r1,_) <- foldBuilder 0 index Do' stmts
      return r1
  build (Lit _ lit) index =
    do
      r1 <- build lit 0
      return (setLeafHeads Lit' r1 index)
  build (Paren _ p) index =
    do
      r1 <- build p 0
      return (setLeafHeads Paren' r1 index)
  build (Con _ qName) index =
    do
      r1 <- build qName 0
      return (setLeafHeads Con' r1 index)
  build (List _ exps) index =
    do
      (r, _) <- foldBuilder 0 index List' exps
      return r
  build (Let _ bind exp) index =
    do
      r1 <- build bind 0
      r2 <- build exp 1
      return (setLeafHeads Let' (r1 ++ r2) index)
  build (Lambda _ pats exp) index =
    do
      (r1, index') <- foldBuilder 0 index Lambda' pats
      r2           <- build exp index'
      return (r1 ++ setLeafHeads Lambda' r2 index)
  build (Case _ exp alts) index =
    do
      r1     <- build exp 0
      (r2,_) <- foldBuilder 1 index Case' alts
      return (setLeafHeads Case' r1 index  ++ r2)
  build (App _ exp1 exp2) index =
    do
      r1 <- build exp1 0
      r2 <- build exp2 1
      return (setLeafHeads App' (r1 ++ r2) index)
  build a index = Left ("Missing pattern in Exp:" ++ show a)

foldBuilder :: (Builder a) => Int -> Int -> Identifier -> [a SrcSpanInfo] -> Either String ([Node], Int)
foldBuilder width index ident xs =
  do
      (nodes, indexCount) <- foldl (\results x ->
          do
            (r, width') <- results
            nodes       <- build x width'
            return (r ++ nodes, width' + 1)
          )
          (Right ([], width)) xs
      return (setLeafHeads ident nodes index, indexCount)

    --foldl (\(r, width') x -> (r ++ (build x width'), width' + 1)) ([], width) xs


foldBuilder' :: (Builder a) => Int -> Identifier -> [a SrcSpanInfo] -> Either String ([Node], Int)
foldBuilder' width ident xs =
  do
      (nodes, indexCount) <- foldl (\results x ->
          do
            (r, width') <- results
            nodes       <- build x width'
            return (r ++ nodes, width' + 1)
          )
          (Right ([], width)) xs
      return (map (\n -> setIndentHead n ident) nodes, indexCount)

instance Builder Binds where
  build (BDecls _ ipBinds) index =
    do
      (r, _) <- foldBuilder 0 index BDecls' ipBinds
      return r
  build (IPBinds _ decls) index  =
    do
      (r, _) <- foldBuilder 0 index IPBinds' decls
      return r

instance Builder IPName where
  build (IPDup _ s) index = return [Leaf [IPDup'] [index] 0 s]
  build (IPLin _ s) index = return [Leaf [IPLin'] [index] 0 s]

instance Builder IPBind where
  build (IPBind _ ipName exp) index =
    do
        r1 <- build ipName 0
        r2 <- build exp 1
        return (setLeafHeads IPBind' (r1 ++ r2) index)

instance Builder Alt where
  build (Alt _ pat rhs Nothing) index =
    do
        r1' <- build pat 0
        r2' <- build rhs 1
        return (setLeafHeads Alt' (r1' ++ r2') index)

instance Builder Literal where
  build (Int _ i' s) index    = return [Leaf [Int'] [index] 0 s]
  build (String _ s s') index = return [Leaf [String'] [index] 0 (show s)]
  build (Char _ c s) index    = return [Leaf [Char'] [index] 0 s]
  build (Frac _ f s) index    = return [Leaf [Frac'] [index] 0 s]
  build a _                   = Left ("Unkown literal: " ++ show a)

instance Builder QOp where
  build (QVarOp _ qname) index =
    do
        r1 <- build qname 0
        return (setLeafHeads QVarOp' r1 index)
  build (QConOp _ qname) index =
    do
        r1 <- build qname 0
        return (setLeafHeads QConOp' r1 index)


instance Builder QName where
  build (UnQual _ name) index =
    do
        r1 <- build name 0
        return (setLeafHeads UnQual' r1 index)
  build (Special _ con) index =
    do
        r1 <- build con 0
        return (setLeafHeads Special' r1 index)
  build a index               = Left ("UnkownError in QName: " ++ show a)

instance Builder SpecialCon where
  build (ListCon _) index  = return [Leaf [ListCon'] [index] 0 "[]"]
  build (Cons _) index     = return [Leaf [Cons'] [index] 0 ":"]
  build (UnitCon _) index  = return [Leaf [UnitCon'] [index] 0 "()"]
  build (FunCon _) index   = return [Leaf [FunCon'] [index] 0 "->"]
  build (ExprHole _) index = return [Leaf [ExprHole'] [index] 0 "_"]
  build a index            = Left ("UnkownError in special con: " ++ show a)

instance Builder Name where
  build (Ident _ name) index   = return [Leaf [Ident'] [index] 0 name]
  build (Symbol  _ name) index = return [Leaf [Symbol'] [index] 0 name]

instance Show Identifier where
  show Match'        = "Match"
  show InfixMatch'   = "MethodDecl"
  show PVar'         = "PVar"
  show PLit'         = "PLit"
  show PList'        = "PList"
  show Signless'     = "Signless"
  show Negative'     = "Negative"
  show UnGuardedRhs' = "UnGuardedRhs"
  show GuardedRhss'  = "GuardedRhss"
  show GuardedRhs'   = "GuardedRhs"
  show Generator'    = "Generator"
  show Qualifier'    = "Qualifier"
  show RecStmt'      = "RecStmt"
  show If'           = "If"
  show InfixApp'     = "InfixApp"
  show Var'          = "Var"
  show BDecls'       = "BDecls"
  show Lit'          = "Lit"
  show Int'          = "Int"
  show String'       = "String"
  show Char'         = "Char"
  show Frac'         = "Frac"
  show QVarOp'       = "QVarOp"
  show QConOp'       = "QConOp"
  show UnQual'       = "UnQual"
  show Ident'        = "Ident"
  show Unqual'       = "Unqual"
  show Symbol'       = "Symbol"
  show QName'        = "QName"
  show Lambda'       = "Lambda"
  show Alt'          = "Alt"
  show Case'         = "Case"
  show App'          = "App"
  show Paren'        = "Paren"
  show Con'          = "Con"
  show List'         = "List"
  show IPDup'        = "IPDup"
  show IPLin'        = "IPLin"
  show IPBind'       = "IPBind"
  show IPBinds'      = "IPBinds"
  show Let'          = "Let"
  show PatBind'      = "PatBind"
  show PParen'       = "PParen"
  show PApp'         = "PApp"
  show PInfixApp'    = "PInfixApp"
  show ListCon'      = "ListCon"
  show ExprHole'     = "ExprHole"
  show Special'      = "Special"
  show Cons'         = "Cons"
  show UnitCon'      = "UnitCon"
  show FunCon'       = "FunCon"
  show PTuple'       = "PTuple"
  show PWildCard'    = "PWildCard"
  show LetStmt'      = "LetStmt"
  show Do'           = "LetStmt"

data Identifier =
  Match'
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
