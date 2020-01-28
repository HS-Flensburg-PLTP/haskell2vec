module Main where
import           AST_Builder
import           Data.Maybe                   (catMaybes)
import           HaskellFilesFinder
import           Language.Haskell.Exts
import           Language.Haskell.Exts.Parser
import           PathExtractor
import           System.FilePath              (takeBaseName)

demoFilePath :: String
demoFilePath = "D:/User/Florian/Desktop/Studium/Semester 7/BachelorArbeit/Project/path-extractor/src/Demo.hs"

demoProjektPath :: String
demoProjektPath = "D:/User/Florian/Desktop/Studium/Semester 7/BachelorArbeit/TestProjekt1"
--"D:/User/Florian/Desktop/Studium/Semester 7/BachelorArbeit/Project/path-extractor/TestProjekt"

extractPathFromTestProjekt :: IO ()
extractPathFromTestProjekt = extractPathFromProject demoProjektPath

extractPathFromProject :: FilePath -> IO()
extractPathFromProject path = do
  ps <- getPathFromProject path
  writeFile "output.c2v" (unlines (map show ps))

showPathFromTestProjekt :: IO ()
showPathFromTestProjekt = showPathFromProject demoProjektPath

showPathFromProject :: FilePath -> IO()
showPathFromProject path = do
  ps <- getPathFromProject path
  putStrLn (unlines (map show ps))

getPathFromProject :: FilePath -> IO [FunctionPath]
getPathFromProject path =
  do
  fs <- getAllFilesFrom path
  foldr (\f r -> do
    r' <- r
    paths <- extractPathFrom f
    case paths of
      (Right ps) -> return (r' ++ ps)
      _          -> return r')
      (return []) fs

extractPathFrom :: FilePath -> IO (Either String [FunctionPath])
extractPathFrom path = do
  eNodes <- getNodesFromFile path
  case eNodes of
    (Right ns) -> return (Right(map extractPaths ns))
    (Left x)   -> return (Left x)


extractPathFromTestFile :: IO (Either String [FunctionPath])
extractPathFromTestFile = extractPathFrom demoFilePath

getNodesFromFile :: FilePath -> IO (Either String [FunctionNodes])
getNodesFromFile f = do
    eDecls <- getDeclsFrom f
    case eDecls of
      Right decls -> return (Right(catMaybes (map buildDecl decls)))
      Left e      -> return (Left e)

getDeclsFrom :: FilePath -> IO (Either String [Decl SrcSpanInfo])
getDeclsFrom f = do
  parseResult <- parseFile f
  case parseResult of
    (ParseOk parse)       -> return (declsFromModule parse)
    (ParseFailed _ error) -> return (Left("Parse error:" ++ error))
  where
  declsFromModule :: Module SrcSpanInfo -> Either String [Decl SrcSpanInfo]
  declsFromModule (Module _ _ _ _ ds) = Right ds
  declsFromModule x                   = Left ("Module not supported: " ++ show x)

showErrorsFromTestProjekt :: IO ()
showErrorsFromTestProjekt = showErrorsFromProject demoProjektPath

showErrorsFromProject :: FilePath -> IO()
showErrorsFromProject path = do
  ps <- getErrorsFromProject path
  putStrLn (unlines (map show ps))

getErrorsFromProject :: FilePath -> IO [String]
getErrorsFromProject path = do
  fs <- getAllFilesFrom path
  foldr (\f r -> do
    r' <- r
    errors <- getErrorsFrom f
    return (r' ++ errors))
    (return []) fs

getErrorsFrom :: FilePath -> IO [String]
getErrorsFrom f = do
  eDecls <- getDeclsFrom f
  case eDecls of
    Right decls -> return (concat(catMaybes(map getBuildErrors decls)))
    Left e      -> return [e]

main :: IO ()
main = putStrLn ("Hello Haskell")
