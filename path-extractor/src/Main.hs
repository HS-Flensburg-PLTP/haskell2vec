module Main where
import           AST_Builder
import           Data.Maybe                   (catMaybes)
import           HaskellFilesFinder
import           Language.Haskell.Exts
import           Language.Haskell.Exts.Parser
import           PathExtractor


demoFilePath :: String
demoFilePath = "D:/User/Florian/Desktop/Studium/Semester 7/BachelorArbeit/Project/path-extractor/src/Demo.hs"

upIdent :: String
upIdent = " U "
-- ↑

downIdent :: String
downIdent = " D "
-- ↓

extractPathFromProject :: FilePath -> IO()
extractPathFromProject path =
  do
  fs <- getAllFilesFrom path
  result <- foldr (\f r ->
                do
                r' <- r
                pathCs <- extractPathFrom f
                return (r'++pathCs))
                (return []) (fs)
  putStrLn (show (result))

extractPathFromDefault :: IO [FunctionPath]
extractPathFromDefault = extractPathFrom demoFilePath

extractPathFrom :: FilePath -> IO [FunctionPath]
extractPathFrom s =
  parseFileWithMode (ParseMode "Demo" Haskell2010 [] False False Nothing False) s
  >>= (\a -> case a of
  (ParseOk p) -> case p of
      (Module _ head pragma imports ds) ->
        --writeFile "output.c2v" (
         return (map extractPaths (map buildDecl ds)))


extractPath'' :: IO ()
extractPath'' = parseFileWithMode (ParseMode "Demo" Haskell2010 [] False False Nothing False) demoFilePath >>= (\a -> case a of
  (ParseOk p) -> case p of
      (Module _ head pragma imports ds) -> putStrLn (show (map buildDecl ds))
      )

extractPath' :: IO ()
extractPath' = parseFileWithMode (ParseMode "Demo" Haskell2010 [] False False Nothing False) demoFilePath >>= (\a -> case a of
  (ParseOk p) -> case p of
      (Module _ head pragma imports ds) -> putStrLn (show (map buildDecl ds))
      )


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
showExp (If _ e1 e2 e3)      = showExp e1 ++ showExp e2 ++ showExp e3
showExp (InfixApp _ e1 q e2) = showExp e1 ++ show q ++ showExp e2
showExp a                    = show a

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
