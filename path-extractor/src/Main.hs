module Main where
import           Language.Haskell.Exts
import           Language.Haskell.Exts.Parser
import           Data.Maybe (catMaybes)

data Node =
  Root String [Node] Int
  | Node String Node [Node] Int Int
  | Leaf String Node Int
  --double linked "list" based on comment in this thread (https://www.reddit.com/r/haskell/comments/2nepr0/implementing_doubly_linked_lists_in_haskell/cmdadok/)
  --  name  parent  children siblingIndex

type NodeBuilder a = Node -> Int -> Int -> a SrcSpanInfo -> (Node, Int)

--cant pass a state due to "tying the knot" strat (?). this is implemented to achieve a double linked list
--which was used to fit the definition of an abstract syntax tree as definded in "A General PathBased Aproach" 4.1

instance Eq Node where
  (Root _ _ id) == (Root _ _ id') = id == id'
  (Node _ _ _ _ id) == (Node _ _ _ _ id') = id == id'
  (Leaf _ _ id) == (Leaf _ _ id') = id == id'
  x == y = False

data PathContext =
  PathContect Node String Node

nameOf :: Node -> String
nameOf (Root name _ _) = name
nameOf (Node name _ _ _ _) = name
nameOf (Leaf name _ _) = name

instance Show Node where
  show (Root name ns id) = name ++ "; id:" ++ show id  ++ show ns
  show (Node name n ns i id) = name ++ "; id:" ++ show id ++ "; siblingIndex:" ++ show i ++ show ns --  ++ show n
  show (Leaf name n id) = name ++ "; id:" ++ show id -- ++ show n

nodeName :: Node -> String
nodeName (Root name _ _) = name
nodeName (Node name _ _ _ _) = name
nodeName (Leaf name _ _) = name

data AbstractSyntaxTree =
  Tree Node
  | Failed

parentOf :: Node -> Maybe Node
parentOf (Root _ _ _) = Nothing
parentOf (Node _ p _ _ _) = Just p
parentOf (Leaf _ p _) = Just p

data ExtractionInfo =
  ExtractionInfo Node [Node] [Node] AbstractSyntaxTree

instance Show ExtractionInfo where
  show (ExtractionInfo r ts nts t) = show r ++ "\n\n" ++ foldl (++) "" (map nodeName ts) ++ "\n\n" ++ foldl (++) "" (map nodeName nts)

instance Show AbstractSyntaxTree where
  show (Tree n) = show n
  show Failed = "Failed building tree"

methodIdentifier :: String
methodIdentifier = "MethodDecl"

data Options =
  Options Int Int Bool
  --width length includeSemiPath

demoFilePath :: String
demoFilePath = "C:/Users/flori/OneDrive/Desktop/Bachelor/path_extractor/path-extractor/src/Demo.hs"

upIdent :: String
upIdent = " U "
-- ↑

downIdent :: String
downIdent = " D "
-- ↓

defaultOptions :: Options
defaultOptions = (Options 2 2 False)

extractPathFromDefault :: IO()
extractPathFromDefault = extractPathFrom demoFilePath

extractPathFrom :: String -> IO()
extractPathFrom s =
  parseFileWithMode (ParseMode "Demo" Haskell2010 [] False False Nothing False) s
  >>= (\a -> case a of
  (ParseOk p) -> case p of
      (Module _ head pragma imports (ds)) ->
        --writeFile "output.txt" (
        putStrLn (
          show (
            map extractPath (catMaybes (map buildExtractionInfo (concat (map buildDecl ds))))))
      )


extractPath :: ExtractionInfo -> [String]
extractPath e = extractPathWithOptions e defaultOptions

extractPathWithOptions :: ExtractionInfo -> Options -> [String]
extractPathWithOptions e@(ExtractionInfo _ ts nts _) (Options l w semi) =
  let
  connections = pathNodes ts
  in
  map (\(n1, n2) -> buildPath l n1 n2) connections

pathNodes :: [Node] -> [(Node,Node)]
pathNodes [] = []
pathNodes (n : ns) = buildTuples n ns ++ pathNodes ns

buildTuples :: a -> [a] -> [(a,a)]
buildTuples x xs = map (\x' -> (x,x')) xs

buildPathContext :: Node -> String -> Node -> String
buildPathContext n1 path n2 =
  "{" ++ nameOf n1 ++ ",(" ++ path ++ ")," ++ nameOf n2 ++ "}"

buildPath :: Int -> Node -> Node -> String
buildPath l n1 n2 = let
  (b, path) = buildPath' [] [] True l n1 n2
  in
  if b then
    buildPathContext n2 path n1
  else
    buildPathContext n1 path n2

buildPath'  :: [(Node, Int)] -> [(Node, Int)] -> Bool -> Int -> Node -> Node -> (Bool, String)
buildPath' ls rs b l root@(Root _ _ _) r =
  buildPath' rs ls (not b) l r root
buildPath' ls rs b l (Leaf _ p _) r =
  let
  ls' = ls ++ [(p, 0)]
  in
  buildPath' rs ls' (not b) l r p
buildPath' ls rs b l n@(Node _ p _ _ _) r =
  let
  newIndex = index ls + 1
  in
  if newIndex > l
  then
    (b,"Path too long")
  else
    if elem p (map fst rs)
    then
      --"listLength:" ++ show newIndex ++ "connected with same parent " ++ nameOf p ++ "." ++ (nameOf . fst . head) rs++ "." ++ (nameOf . fst . head) ls
      --"rs:" ++ show rs ++ "ls " ++ show ls
      if b
      then
        (b,connectPath rs ls l p)
      else
        (b,connectPath ls rs l p)
    else
      buildPath' rs (ls ++ [(p, newIndex)]) (not b) l r p

connectPath :: [(Node, Int)] -> [(Node, Int)] -> Int -> Node -> String
connectPath ls rs l p =
  let
  (p1, l1) = buildToParent ls p (\r s -> r ++ s ++ upIdent) ""
  (p2, l2) = buildToParent rs p (\r s -> downIdent ++ s ++ r) ""
  totalLength = l1 + l2
  in
  if(totalLength > l) then
    "Path too long" ++ show totalLength ++ "/" ++ show l
  else
    p1 ++ nameOf p ++ p2 -- ++ ";Path length left " ++ show l1 ++ " path length right :" ++ show l2

buildToParent :: [(Node, Int)] -> Node -> (String -> String -> String) -> String -> (String, Int)
buildToParent [] _ _ r = (r,0)
buildToParent ((n,i) : []) p builder r = if n == p
  then (r,i) --i-1?
  else (builder r (nameOf n),i)
buildToParent ((n,i) : ns) p builder r = if n == p
  then (r,i)
  else buildToParent ns p builder (builder r (nameOf n))

index :: [(Node, Int)] -> Int
index [] = 0
index ((_, i) : _) = i

buildExtractionInfo :: AbstractSyntaxTree-> Maybe ExtractionInfo
buildExtractionInfo Failed = Nothing
buildExtractionInfo t@(Tree r) = Just(buildExtractionInfos [r] 0 (ExtractionInfo r [] [r] t))

buildExtractionInfos :: [Node] -> Int -> ExtractionInfo -> ExtractionInfo
buildExtractionInfos [] _ e = e
buildExtractionInfos (n : ns) i (ExtractionInfo r ts nts t) =
  case n of
    r@(Root _ cs _) -> buildExtractionInfos cs 0 (ExtractionInfo r ts nts t)
    n@(Node _ _ cs _ _) ->
      let newInfo = buildExtractionInfos cs 0 (ExtractionInfo r ts (n : nts) t)
      in buildExtractionInfos ns (i + 1) newInfo
    l@(Leaf _ _ _) -> (ExtractionInfo r (l:ts) nts t)

extractPath'' :: IO ()
extractPath'' = parseFileWithMode (ParseMode "Demo" Haskell2010 [] False False Nothing False) demoFilePath >>= (\a -> case a of
  (ParseOk p) -> case p of
      (Module _ head pragma imports ds) -> putStrLn ( show ( map buildDecl ds))
      )

extractPath' :: IO ()
extractPath' = parseFileWithMode (ParseMode "Demo" Haskell2010 [] False False Nothing False) demoFilePath >>= (\a -> case a of
  (ParseOk p) -> case p of
      (Module _ head pragma imports (d1:d2:d3:ds)) -> putStrLn (show (map buildExtractionInfo (buildDecl d1)))
      )

buildDecl :: Decl SrcSpanInfo -> [AbstractSyntaxTree]
buildDecl (FunBind _ ms) = map buildMethod ms
buildDecl _ = [Failed]

buildMethod :: Match SrcSpanInfo -> AbstractSyntaxTree
buildMethod (Match _ name ps rhs _) = Tree root
    where root = (Root methodIdentifier children 0)
          children = pattern ++ [fst (buildRhs root index id1 rhs)]
          (pattern, id1, index) = buildPattern root [] 1 0 ps

buildPattern :: Node -> [Node] -> Int -> Int -> [Pat SrcSpanInfo] -> ([Node], Int, Int)
buildPattern _ ns id i [] = (ns, id,i)
buildPattern r ns id i (p : ps) = buildPattern r (node : ns) id1 (i + 1) ps
  where (node, id1) = buildPat r i id p


buildPat ::  NodeBuilder Pat
buildPat parent i id (PVar _ name) = (node, id1)
  where node = (Node "PVar" parent [child] i id)
        (child, id1) = buildName node 0 (id + 1) name
buildPat parent i id (PLit _ sign lit) = (node, id2)
  where node = (Node "PLit" parent [child1, child2] i id)
        (child1, id1) = buildSign node 0 (id + 1) sign
        (child2, id2) = buildLit node 1 (id1) lit
--buildNode parent i id p = (Leaf (show p) parent id, id+1)
--

buildSign :: NodeBuilder Sign
buildSign p i id (Signless _) = (node , id + 2)
  where node = (Node "Signless" p [child] i id)
        child = (Leaf "" p (id + 1))

--buildPVar :: NodeBuilder PVar
--buildPVar

buildRhs :: NodeBuilder Rhs
buildRhs parent i id (UnGuardedRhs _ exp) = (node, newId)
    where node = (Node "UnGuardedRhs" parent [child] i id)
          (child,newId) = buildExpr node 0 (id + 1) exp

buildExpr :: NodeBuilder Exp
buildExpr parent i id (If _ exp1 exp2 exp3) = (node, id3)
    where node = (Node "If" parent [node1, node2, node3] i id)
          (node1, id1) = buildExpr node 0 (id + 1) exp1
          (node2, id2) = buildExpr node 1 id1 exp2
          (node3, id3) = buildExpr node 2 id2 exp3
buildExpr parent i id (InfixApp _ exp1 qop exp2) = (node, id3)
    where node = (Node "InfixApp" parent [node1,node2, node3] i id)
          (node1, id1) = buildExpr node 0 (id + 1) exp1
          (node2, id2) = buildQOp node 1 id1 qop
          (node3, id3) = buildExpr node 2 id2 exp2
buildExpr parent i id (Var _ qname) = (node, id1)
    where node = (Node "Var" parent [node1] i id)
          (node1, id1) = buildQName node 0 (id + 1) qname
buildExpr parent i id (Lit _ lit) = (node,id1)
    where node = (Node "InfixApp" parent [node1] i id)
          (node1, id1) = buildLit node 0 (id+1) lit
buildExpr parent i id a = (Leaf (show a) parent id, id + 1)

buildLit :: NodeBuilder Literal
buildLit p i id (Int _ i' s) = (node, id + 2)
    where node = (Node "Int" p [leaf] i id)
          leaf = (Leaf s node (id + 1))
buildLit p i id (String _ s s') = (node, id + 2)
    where node = (Node "String" p [leaf] i id)
          leaf = (Leaf s' node (id + 1))
buildLit p i id (Char _ c s) = (node, id + 2)
    where node = (Node "Char" p [leaf] i id)
          leaf = (Leaf s node (id + 1))
buildLit p i id (Frac _ f s) = (node, id + 2)
    where node = (Node "Frac" p [leaf] i id)
          leaf = (Leaf s node (id + 1))

buildQOp ::  NodeBuilder QOp
buildQOp parent i id (QVarOp _ qname) = (node,id1)
    where node = (Node "QVarOp" parent [node1] i id)
          (node1, id1) = buildQName node 0 (id + 1) qname
buildQOp parent i id (QConOp _ qname) = (node, id1)
    where node = (Node "QConOp" parent [node1] i id)
          (node1, id1) = buildQName node 0 (id + 1) qname

buildQName ::  NodeBuilder QName
buildQName parent i id (UnQual _ name) = (node, id1)
    where node = (Node "UnQual" parent [node1] i id)
          (node1, id1) = buildName node 0 (id + 1) name

buildName :: NodeBuilder Name
buildName parent i id (Ident _ name) = (node, id + 2)
    where node = (Node "Ident" parent [Leaf name node (id + 1)] i id)
buildName parent i id (Symbol  _ name) = (node, id+2)
    where node = (Node "Symbol" parent [Leaf name node (id + 1)] i id)

main :: IO ()
main = putStrLn ("Hello Haskell")

prettyParse :: IO ()
prettyParse = parseFileWithMode (ParseMode "Demo" Haskell2010 [] False False Nothing False) "C:/Users/flori/OneDrive/Desktop/Bachelor/path_extractor/path-extractor/src/Demo.hs" >>= (\a -> case a of
  (ParseOk p) -> case p of
      --(Module m1 m2 m3 m4 m5) -> putStrLn (show m5))
      (Module _ head pragma imports decl) -> putStrLn (foldl (\r a ->r ++ "\n\n" ++ buildString a) "" decl))
       --(TypeSig (SrcSpanInfo s1 s2) t1 t2) -> (putStrLn (show t1)))


buildString :: Decl SrcSpanInfo -> String
buildString (TypeSig _ name t) = "TYPTYPTYP" ++ show name ++ "\n\n" ++ show t --t1 irrelevant -> is method name
buildString (FunBind _ ms) =  "FUNFUNFUN" ++ foldl (\r m -> r ++  "\n\n" ++ showMatch m) "" ms


showMatch :: Match SrcSpanInfo -> String
showMatch (Match _ name pat rhs mBinds) = showRhs rhs


showRhs :: Rhs SrcSpanInfo -> String
showRhs (UnGuardedRhs _ exp) = showExp exp
--buildString a = show a

showExp :: Exp SrcSpanInfo -> String
showExp (If _ e1 e2 e3) = showExp e1 ++ showExp e2 ++ showExp e3
showExp (InfixApp _ e1 q e2) = showExp e1 ++ show q ++ showExp e2
showExp a = show a

{-
Module -> root representation of source code
  SrcSpanInfo -> vermutlich unwichtig
  ModuleHead -> ok
  Module pragma -> muda
  ImportDecl -> egal
  Decl -> main sourcecode
-}
{-
main :: IO ()
main = parseFile "C:/Users/flori/OneDrive/Desktop/Bachelor/path_extractor/path-extractor/src/Demo.hs"
main = parseFileWithMode (ParseMode "Demo" Haskell2010 [] True True Nothing True) "C:/Users/flori/OneDrive/Desktop/Bachelor/path_extractor/path-extractor/src/Demo.hs"
-}

{-

InfixApp (SrcSpanInfo {srcInfoSpan = SrcSpan "Demo" 8 20 8 27, srcInfoPoints = []}) (Var (SrcSpanInfo {srcInfoSpan = SrcSpan "Demo" 8 20 8 23, srcInfoPoints = []}) (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "Demo" 8 20 8 23, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "Demo" 8 20 8 23, srcInfoPoints = []}) "val"))) (QVarOp (SrcSpanInfo {srcInfoSpan = SrcSpan "Demo" 8 24 8 25, srcInfoPoints = []}) (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "Demo" 8 24 8 25, srcInfoPoints = []}) (Symbol (SrcSpanInfo {srcInfoSpan = SrcSpan "Demo" 8 24 8 25, srcInfoPoints = []}) ">"))) (Lit (SrcSpanInfo {srcInfoSpan = SrcSpan "Demo" 8 26 8 27, srcInfoPoints = []}) (Int (SrcSpanInfo {srcInfoSpan = SrcSpan "Demo" 8 26 8 27, srcInfoPoints = []}) 0 "0"))InfixApp (SrcSpanInfo {srcInfoSpan = SrcSpan "Demo" 8 33 8 42, srcInfoPoints = []}) (Var (SrcSpanInfo {srcInfoSpan = SrcSpan "Demo" 8 33 8 36, srcInfoPoints = []}) (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "Demo" 8 33 8 36, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "Demo" 8 33 8 36, srcInfoPoints = []}) "val"))) (QVarOp (SrcSpanInfo {srcInfoSpan = SrcSpan "Demo" 8 37 8 38, srcInfoPoints = []}) (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "Demo" 8 37 8 38, srcInfoPoints = []}) (Symbol (SrcSpanInfo {srcInfoSpan = SrcSpan "Demo" 8 37 8 38, srcInfoPoints = []}) "*"))) (Var (SrcSpanInfo {srcInfoSpan = SrcSpan "Demo" 8 39 8 42, srcInfoPoints = []}) (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "Demo" 8 39 8 42, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "Demo" 8 39 8 42, srcInfoPoints = []}) "val")))InfixApp (SrcSpanInfo {srcInfoSpan = SrcSpan "Demo" 8 48 8 55, srcInfoPoints = []}) (Var (SrcSpanInfo {srcInfoSpan = SrcSpan "Demo" 8 48 8 51, srcInfoPoints = []}) (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "Demo" 8 48 8 51, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "Demo" 8 48 8 51, srcInfoPoints = []}) "val"))) (QVarOp (SrcSpanInfo {srcInfoSpan = SrcSpan "Demo" 8 52 8 53, srcInfoPoints = []}) (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "Demo" 8 52 8 53, srcInfoPoints = []}) (Symbol (SrcSpanInfo {srcInfoSpan = SrcSpan "Demo" 8 52 8 53, srcInfoPoints = []}) "+"))) (Lit (SrcSpanInfo {srcInfoSpan = SrcSpan "Demo" 8 54 8 55, srcInfoPoints = []}) (Int (SrcSpanInfo {srcInfoSpan = SrcSpan "Demo" 8 54 8 55, srcInfoPoints = []}) 1 "1"))




-}
