module AST_Builder where
import           Language.Haskell.Exts
import           Language.Haskell.Exts.Syntax

type NodeBuilder a = [Int] -> [Identifier] -> Int -> a SrcSpanInfo -> [Node]

type Value = String

class Builder a where
  build :: a SrcSpanInfo -> Int -> [Node]

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

buildDecl :: Decl SrcSpanInfo -> FunctionNodes
buildDecl (FunBind _ ms) = FunctionNodes (nameOfFun ms) (map buildMethod ms)

nameOfFun :: [Match SrcSpanInfo] -> String
nameOfFun ((Match _ name _ _ _) : fs) = nameOf name
nameOfFun (f : fs)                    = nameOfFun fs

nameOf :: Name a -> String
nameOf (Ident _ name)  = name
nameOf (Symbol _ name) = name

buildMethod :: Match SrcSpanInfo -> [Node]
buildMethod (Match _ name ps rhs _) = patterns ++ map (\n -> setIndentHead n MethodDecl') rhs'
    where (patterns, index) = foldBuilder' 0 MethodDecl' ps
          rhs' = build rhs index

instance Builder Decl where
  build (PatBind _ pat rhs (Just bind)) index = setLeafHeads PatBind' (r1 ++ r2 ++ r3) index
    where r1 = build pat 0
          r2 = build rhs 1
          r3 = build bind 2
  build (PatBind _ pat rhs Nothing) index = setLeafHeads PatBind' (r1 ++ r2) index
    where r1 = build pat 0
          r2 = build rhs 1
  build a index = [Leaf [PatBind'] [index] 0 (show a)]


instance Builder Pat where
  build (PVar _ name) index = setLeafHeads PVar' (build name 0) index
  build (PLit _ sign lit) index = setLeafHeads PLit' (r1 ++ r2) index
    where r1 = build sign 0
          r2 = build lit 1
  build (PList _ pats) index = fst (foldBuilder 0 index PList' pats)
  build (PParen _ pat) index = setLeafHeads PParen' (build pat 0) index
  build (PTuple _ _ pats) index = fst (foldBuilder 0 index PTuple' pats)
  build (PWildCard _) index = [Leaf [PWildCard'] [index] 0 "_"]
  build (PInfixApp _ pat1 qName pat2) index =
    setLeafHeads PInfixApp' (r1 ++ r2 ++ r3) index
    where
      r1 = build pat1 0
      r2 = build qName 1
      r3 = build pat2 2
  build (PApp _ qName pats) index = r1 ++ r2
    where
      r1 = setLeafHeads PApp' (build qName 0) index
      r2 = fst (foldBuilder 1 index PApp' pats)
  build a index = [Leaf [] [index] 0 ("Error in Pat:" ++ show a)]

--buildNode parent i id p = (Leaf (show p) parent id, id+1)
--

instance Builder Sign where
  build (Signless _) index = [Leaf [Signless'] [index] 0 "+"]
  build (Negative _) index = [Leaf [Negative'] [index] 0 "-"]

--buildPVar :: NodeBuilder PVar
--buildPVar

instance Builder Rhs where
  build (UnGuardedRhs _ exp) index = setLeafHeads UnGuardedRhs' (build exp 0) index
  build (GuardedRhss _ rhss) index =
    fst (foldBuilder 0 index GuardedRhss' rhss)

instance Builder GuardedRhs where
  build (GuardedRhs _ stmts exp) index =
    r1 ++ setLeafHeads GuardedRhs' r2 index
      where
        (r1, width) = foldBuilder 0 index GuardedRhs' stmts
        r2 = build exp width

instance Builder Stmt where
  build (Generator _ pat exp) index = setLeafHeads Generator' (r1 ++ r2) index
      where r1 = build pat 0
            r2 = build exp 1
  build (Qualifier _  exp) index = setLeafHeads Qualifier' (build exp 0) index
  build (RecStmt _ stmts) index = fst (foldBuilder 0 index RecStmt' stmts)

instance Builder Exp where
  build (If _ exp1 exp2 exp3) index = setLeafHeads If' (r1 ++ r2 ++ r3) index
    where r1 = build exp1 0
          r2 = build exp2 1
          r3 = build exp3 2
  build (InfixApp _ exp1 qop exp2) index = setLeafHeads InfixApp' (r1 ++ r2 ++ r3) index
    where r1 = build exp1 0
          r2 = build qop 1
          r3 = build exp2 2
  build (Var _ qname) index = setLeafHeads Var' (build qname 0) index
  build (Lit _ lit) index = setLeafHeads Lit' (build lit 0) index
  build (Paren _ p) index = setLeafHeads Paren' (build p 0) index
  build (Con _ qName) index = setLeafHeads Con' (build qName 0) index
  build (List _ exps) index = fst (foldBuilder 0 index List' exps)
  build (Let _ binds exp) index = setLeafHeads Let' (r1 ++ r2) index
    where
      r1 = build binds 0
      r2 = build exp 1
  build (Lambda _ pats exp) index = patterns ++ setLeafHeads Lambda' exp' index
    where (patterns, index') = foldBuilder 0 index Lambda' pats
          exp' = build exp index'
  build (Case _ exp alts) index = setLeafHeads Case' exp' index ++ alt'
    where
      exp' = build exp 0
      alt' = fst (foldBuilder 1 index Case' alts)
  build (App _ exp1 exp2) index = setLeafHeads App' (r1 ++ r2) index
    where
      r1 = build exp1 0
      r2 = build exp2 1
  build a index = [Leaf [] [index] 0 ("Expression Error:" ++ show a)]

foldBuilder :: (Builder a) => Int -> Int -> Identifier -> [a SrcSpanInfo] -> ([Node], Int)
foldBuilder width index ident xs =
  let
  (nodes, indexCount) =
    foldl (\(r, width') x -> (r ++ (build x width'), width' + 1)) ([], width) xs
  in
  (setLeafHeads ident nodes index, indexCount)

foldBuilder' :: (Builder a) => Int -> Identifier -> [a SrcSpanInfo] -> ([Node], Int)
foldBuilder' width ident xs =
  let
  (nodes, indexCount) =
    foldl (\(r, width') x -> (r ++ (build x width'), width' + 1)) ([], width) xs
  in
  (map (\n -> setIndentHead n ident) nodes, indexCount)

instance Builder Binds where
  build (BDecls _ ipBinds) index = fst (foldBuilder 0 index IPBinds' ipBinds)
  build (IPBinds _ decls) index  = fst (foldBuilder 0 index IPBinds' decls)

instance Builder IPName where
  build (IPDup _ s) index = [Leaf [IPDup'] [index] 0 s]
  build (IPLin _ s) index = [Leaf [IPLin'] [index] 0 s]

instance Builder IPBind where
  build (IPBind _ ipName exp) = setLeafHeads IPBind' (r1 ++ r2)
    where
      r1 = build ipName 0
      r2 = build exp 1

instance Builder Alt where
  build (Alt _ pat rhs Nothing) = setLeafHeads Alt' (r1' ++ r2')
    where
      r1' = build pat 0
      r2' = build rhs 1

instance Builder Literal where
  build (Int _ i' s) index    = [Leaf [Int'] [index] 0 s]
  build (String _ s s') index = [Leaf [String'] [index] 0 s]
  build (Char _ c s) index    = [Leaf [Char'] [index] 0 s]
  build (Frac _ f s) index    = [Leaf [Frac'] [index] 0 s]

instance Builder QOp where
  build (QVarOp _ qname) = setLeafHeads QVarOp' (build qname 0)
  build (QConOp _ qname) = setLeafHeads QConOp' (build qname 0)

instance Builder QName where
  build (UnQual _ name) = setLeafHeads UnQual' (build name 0)
  build (Special _ con) = setLeafHeads Special' (build con 0)

instance Builder SpecialCon where
  build (ListCon _) index     = [Leaf [ListCon'] [index] 0 "[]"]
  build (Cons _) index     = [Leaf [Cons'] [index] 0 ":"]
  build (UnitCon _) index     = [Leaf [UnitCon'] [index] 0 "()"]
  build (FunCon _) index     = [Leaf [FunCon'] [index] 0 "->"]
  build (ExprHole _) index     = [Leaf [ExprHole'] [index] 0 "_"]
  build a index     = [Leaf [ListCon'] [index] 0 ("UnkownError in special con: " ++ show a)]

instance Builder Name where
  build (Ident _ name) index   = [Leaf [Ident'] [index] 0 name]
  build (Symbol  _ name) index = [Leaf [Symbol'] [index] 0 name]

instance Show Identifier where
  show MethodDecl'   = "MethodDecl"
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

data Identifier =
  MethodDecl'
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
