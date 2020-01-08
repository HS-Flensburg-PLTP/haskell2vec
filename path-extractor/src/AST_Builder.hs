module AST_Builder where
import           Language.Haskell.Exts
import           Language.Haskell.Exts.Syntax

type NodeBuilder a = [Int] -> [Identifier] -> Int -> a SrcSpanInfo -> [Node]

type Value = String

class Builder a where
  build :: [Int] -> [Identifier] -> Int -> a SrcSpanInfo -> [Node]

data Node =
    Leaf [Identifier] [Int] Int Value

data FunctionNodes =
  FunctionNodes String [[Node]]

instance Show FunctionNodes where
  show (FunctionNodes s nss) = s ++ concatMap show nss

instance Show Node where
  show (Leaf idents is len v) = "Identifications:" ++ show idents ++ "; Siblindindicies:" ++ show is ++ "; Length:" ++ show len ++ "; Value:" ++ v

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
buildMethod (Match _ name ps rhs _) = patterns ++ rhs'
    where (patterns, index) = foldBuilder 0 [] [MethodDecl'] 0 ps
          rhs' = build [index] [MethodDecl'] 1 rhs

instance Builder Decl where
  build is idents len (PatBind _ pat rhs (Just bind)) = r1 ++ r2 ++ r3
    where r1 = build (is ++ [0]) idents' (len + 1) pat
          r2 = build (is ++ [1]) idents' (len + 1) rhs
          r3 = build (is ++ [2]) idents' (len + 1) bind
          idents' = idents ++ [PatBind']
  build is idents len (PatBind _ pat rhs Nothing) = r1 ++ r2
    where r1 = build (is ++ [0]) idents' (len + 1) pat
          r2 = build (is ++ [1]) idents' (len + 1) rhs
          idents' = idents ++ [PatBind']
  build is idents len a = [Leaf idents is len (show a)]


instance Builder Pat where
  build is idents len (PVar _ name) = build (is ++ [0]) (idents ++ [PVar']) (len + 1) name
  build is idents len (PLit _ sign lit) = r1 ++ r2
    where r1 = build (is ++ [0]) idents' (len + 1) sign
          r2 = build (is ++ [1]) idents' (len + 1) lit
          idents' = idents ++ [PLit']
  build is idents len (PList _ pats) = fst (foldBuilder 0 is idents len pats)
  build is idents len a = [Leaf idents is len (show a)]

--buildNode parent i id p = (Leaf (show p) parent id, id+1)
--

instance Builder Sign where
  build is idents len (Signless _) = [Leaf (idents ++ [Signless']) is len ""]

--buildPVar :: NodeBuilder PVar
--buildPVar

instance Builder Rhs where
  build is idents len (UnGuardedRhs _ exp) =
    build (is ++ [0]) (idents ++ [UnGuardedRhs']) (len + 1) exp
  build is idents len (GuardedRhss _ rhss) =
    fst (foldBuilder 0 is (idents ++ [GuardedRhss']) len rhss)

instance Builder GuardedRhs where
  build is idents len (GuardedRhs _ stmts exp) =
    nodes ++ build (is ++ [width]) idents' (len + 1) exp
      where
        (nodes, width) = foldBuilder 0 is (idents ++ [GuardedRhs']) len stmts
        idents' = idents ++ [GuardedRhs']

instance Builder Stmt where
  build is idents len (Generator _ pat exp) = child1 ++ child2
      where child1 = build (is ++ [0]) idents' (len + 1) pat
            child2 = build (is ++ [1]) idents' (len + 1) exp
            idents' = idents ++ [Generator']
  build is idents len (Qualifier _  exp) =
    build (is ++ [0]) (idents ++ [Qualifier']) (len + 1) exp
  build is idents len (RecStmt _ stmts) = fst (foldBuilder 0 is (idents ++ [RecStmt']) len stmts)

instance Builder Exp where
  build is idents len (If _ exp1 exp2 exp3) = node1 ++ node2 ++ node3
    where node1 = build (is ++ [0]) idents' (len + 1) exp1
          node2 = build (is ++ [1]) idents' (len + 1) exp2
          node3 = build (is ++ [2]) idents' (len + 1) exp3
          idents' = idents ++ [If']
  build is idents len (InfixApp _ exp1 qop exp2) = node1 ++ node2 ++ node3
    where node1 = build (is ++ [0]) idents' (len + 1) exp1
          node2 = build (is ++ [1]) idents' (len + 1) qop
          node3 = build (is ++ [2]) idents' (len + 1) exp2
          idents' = idents ++ [InfixApp']
  build is idents len (Var _ qname) = build (is ++ [0]) (idents ++ [Var']) (len + 1) qname
  build is idents len (Lit _ lit) = build (is ++ [0]) (idents ++ [Lit']) (len + 1) lit
  build is idents len (Paren _ p) = build (is ++ [0]) (idents ++ [Paren']) (len + 1) p
  build is idents len (Con _ qName) = build (is ++ [0]) (idents ++ [Con']) (len + 1) qName
  build is idents len (List _ exps) = fst (foldBuilder 0 is (idents ++ [List']) len exps)
  build is idents len (Let _ binds exp) = r1 ++ r2
    where
      r1 = build (is ++ [0]) idents' (len + 1) binds
      r2 = build (is ++ [1]) idents' (len + 1) exp
      idents' = idents ++ [Let']
  build is idents len (Lambda _ pats exp) = patterns ++ exp'
    where (patterns, index) = foldBuilder 0 is idents' len pats
          exp' = build (is ++ [index]) idents' (len + 1) exp
          idents' = idents ++ [Lambda']
  build is idents len (Case _ exp alts) = exp' ++ alt'
    where
      exp' = build (is ++ [0]) idents' (len + 1) exp
      alt' = fst (foldBuilder 1 is idents' len alts)
      idents' = idents ++ [Case']
  build is idents len (App _ exp1 exp2) = r1 ++ r2
    where
      r1 = build (is ++ [0]) idents' (len + 1) exp1
      r2 = build (is ++ [1]) idents' (len + 1) exp2
      idents' = idents ++ [App']
  build is idents len a = [Leaf idents is len ("Exp Error:" ++ show a)]

foldBuilder :: (Builder a) => Int -> [Int] -> [Identifier] -> Int -> [a SrcSpanInfo] -> ([Node], Int)
foldBuilder width is idents len =
  foldl (\(r, width') x -> (r ++ (build (is ++ [width']) idents (len + 1) x), width' + 1)) ([], width)

instance Builder Binds where
  build is idents len (BDecls _ ipBinds) = fst (foldBuilder 0 is (idents ++ [IPBinds']) len ipBinds)
  build is idents len (IPBinds _ decls) = fst (foldBuilder 0 is (idents ++ [IPBinds']) len decls)

instance Builder IPName where
  build is idents len (IPDup _ s) = [Leaf (idents ++ [IPDup']) is len s]
  build is idents len (IPLin _ s) = [Leaf (idents ++ [IPLin']) is len s]

instance Builder IPBind where
  build is idents len (IPBind _ ipName exp) = r1 ++ r2
    where
      r1 = build (is ++ [0]) idents' (len + 1) ipName
      r2 = build (is ++ [1]) idents' (len + 1) exp
      idents' = idents ++ [IPBind']

instance Builder Alt where
  build is idents len (Alt _ pat rhs Nothing) = pat' ++ rhs'
    where
      pat' = build (is ++ [0]) idents' (len + 1) pat
      rhs' = build (is ++ [1]) idents' (len + 1) rhs
      idents' = idents ++ [Alt']

instance Builder Literal where
  build is idents len (Int _ i' s)    = [Leaf (idents ++ [Int']) is len s]
  build is idents len (String _ s s') = [Leaf (idents ++ [String']) is len s]
  build is idents len (Char _ c s)    = [Leaf (idents ++ [Char']) is len s]
  build is idents len (Frac _ f s)    = [Leaf (idents ++ [Frac']) is len s]

instance Builder QOp where
  build is idents len (QVarOp _ qname) = build (is ++ [0]) (idents ++ [QVarOp']) (len + 1) qname
  build is idents len (QConOp _ qname) = build (is ++ [0]) (idents ++ [QConOp']) (len + 1) qname

instance Builder QName where
  build is idents len (UnQual _ name) = build (is ++ [0]) (idents ++ [UnQual']) (len + 1) name

instance Builder Name where
  build is idents len (Ident _ name)   = [Leaf (idents ++ [Ident']) is len name]
  build is idents len (Symbol  _ name) = [Leaf (idents ++ [Symbol']) is len name]

instance Show Identifier where
  show MethodDecl'   = "MethodDecl"
  show PVar'         = "PVar"
  show PLit'         = "PLit"
  show Signless'     = "Signless"
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

data Identifier =
  MethodDecl'
  | PVar'
  | PLit'
  | Signless'
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
