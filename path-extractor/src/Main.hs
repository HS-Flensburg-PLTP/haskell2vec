module Main where
import           AST_Builder
import           Data.Maybe                   (catMaybes)
import           HaskellFilesFinder
import           Language.Haskell.Exts
import           Language.Haskell.Exts.Parser
import           PathExtractor

demoFilePath :: String
demoFilePath = "D:/User/Florian/Desktop/Studium/Semester 7/BachelorArbeit/Project/path-extractor/src/Demo.hs"

demoProjektPath :: String
demoProjektPath = "D:/User/Florian/Desktop/Studium/Semester 7/BachelorArbeit/Project/path-extractor/TestProjekt3"
--"D:/User/Florian/Desktop/Studium/Semester 7/BachelorArbeit/Project/path-extractor/TestProjekt"

extractPathFromTestProjekt :: IO ()
extractPathFromTestProjekt = extractPathFromProject demoProjektPath

extractPathFromProject :: FilePath -> IO()
extractPathFromProject path =
  do
  ps <- getPathFromProject path
  writeFile "output.c2v" (unlines (map show ps))

showPathFromTestProjekt :: IO ()
showPathFromTestProjekt = showPathFromProject demoProjektPath

showPathFromProject :: FilePath -> IO()
showPathFromProject path =
  do
  ps <- getPathFromProject path
  putStrLn (unlines (map show ps))

getPathFromProject :: FilePath -> IO [FunctionPath]
getPathFromProject path =
  do
  fs <- getAllFilesFrom path
  foldr (\f r ->
                do
                r' <- r
                paths <- extractPathFrom f
                case paths of
                  (Right ps) -> return (r' ++ ps)
                  _          -> r)
                (return []) fs

showErrorsFromTestProjekt :: IO ()
showErrorsFromTestProjekt = showErrorsFromProject demoProjektPath

showErrorsFromProject :: FilePath -> IO()
showErrorsFromProject path =
  do
  ps <- getErrorsFromProject path
  putStrLn (unlines (map show ps))

getErrorsFromProject :: FilePath -> IO [String]
getErrorsFromProject path =
  do
  fs <- getAllFilesFrom path
  foldr (\f r ->
                do
                r' <- r
                errors <- getErrorsFrom f
                return (r' ++ errors)
        ) (return []) fs

showBuildErrorsFromTestProjekt :: IO ()
showBuildErrorsFromTestProjekt = showBuildErrorsInProjekt demoProjektPath

showBuildErrorsInProjekt :: FilePath -> IO()
showBuildErrorsInProjekt path =
  do
  ps <- getPathFromProject path
  putStrLn (unlines (map show ps))

getErrorsFrom :: FilePath -> IO [String]
getErrorsFrom s =
  parseFileWithMode defaultParseMode s
  >>= (\a -> case a of
  (ParseOk p) -> case p of
      (Module _ _ _ _ ds) ->
         return (concat(catMaybes(map getBuildErrors ds)))
  x -> return [show x])

extractPathFromDefault :: IO (Either String [FunctionPath])
extractPathFromDefault = extractPathFrom demoFilePath

extractPathFrom :: FilePath -> IO (Either String [FunctionPath])
extractPathFrom path =
  do
    eNodes <- getNodesFromFile path
    case eNodes of
      (Right ns) -> return (Right(map extractPaths ns))
      (Left x)   -> return (Left x)

getNodesFromFile :: FilePath -> IO (Either String [FunctionNodes])
getNodesFromFile f =
  do
    parse <- parseFileWithMode defaultParseMode f
    case parse of
        (ParseOk p) -> case p of
           (Module _ _ _ _ ds) -> return (Right(catMaybes (map buildDecl ds)))
        x -> return (Left ("No Pattern in getNodesFromFile for:" ++ show x))

main :: IO ()
main = putStrLn ("Hello Haskell")
