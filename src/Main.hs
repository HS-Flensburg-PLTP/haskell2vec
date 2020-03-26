module Main where

import Data.Either
import Data.Maybe (catMaybes)
import HaskellFilesFinder
import Language.Haskell.Exts
import Language.Haskell.Exts.Parser
import LeafCollector
import PathExtractor
import System.FilePath (takeBaseName)

demoProjektPath :: String
demoProjektPath =
  "D:/User/Florian/Desktop/Studium/Semester 7/BachelorArbeit/TestProjekt1"

--"D:/User/Florian/Desktop/Studium/Semester 7/BachelorArbeit/Project/path-extractor/TestProjekt"
extractPathsFromTestProjekt :: IO ()
extractPathsFromTestProjekt = extractPathsFromProject demoProjektPath

extractPathsFromProject :: FilePath -> IO ()
extractPathsFromProject path = do
  ps <- getPathsFromProject path
  writeFile "output.c2s" (unlines (map show ps))

showPathsFromTestProjekt :: IO ()
showPathsFromTestProjekt = showPathsFromProject demoProjektPath

showPathsFromProject :: FilePath -> IO ()
showPathsFromProject path = do
  ps <- getPathsFromProject path
  putStrLn (unlines (map show ps))

getPathsFromProject :: FilePath -> IO [FunctionPaths]
getPathsFromProject path = do
  fs <- getAllFilesFrom path
  foldr
    (\f r -> do
       r' <- r
       paths <- extractPathsFrom f
       case paths of
         (Right ps) -> return (r' ++ ps)
         _ -> return r')
    (return [])
    fs

extractPathsFrom :: FilePath -> IO (Either String [FunctionPaths])
extractPathsFrom path = do
  eNodes <- getNodesFromFile path
  case eNodes of
    (Right ns) -> return (Right (map extractPaths ns))
    (Left x) -> return (Left x)

getNodesFromFile :: FilePath -> IO (Either String [FunctionLeaves])
getNodesFromFile f = do
  eDecls <- getDeclsFrom f
  case eDecls of
    Right decls -> return (Right (rights (map buildDecl decls)))
    Left e -> return (Left e)

getDeclsFrom :: FilePath -> IO (Either String [Decl SrcSpanInfo])
getDeclsFrom f = do
  parseResult <- parseFile f
  case parseResult of
    (ParseOk parse) -> return (declsFromModule parse)
    (ParseFailed _ error) -> return (Left ("Parse error:" ++ error))
  where
    declsFromModule :: Module SrcSpanInfo -> Either String [Decl SrcSpanInfo]
    declsFromModule (Module _ _ _ _ ds) = Right ds
    declsFromModule x = Left ("Module not supported: " ++ show x)

showErrorsFromTestProjekt :: IO ()
showErrorsFromTestProjekt = showErrorsFromProject demoProjektPath

showErrorsFromProject :: FilePath -> IO ()
showErrorsFromProject path = do
  ps <- getErrorsFromProject path
  putStrLn (unlines (map show ps))

getErrorsFromProject :: FilePath -> IO [String]
getErrorsFromProject path = do
  fs <- getAllFilesFrom path
  foldr
    (\f r -> do
       r' <- r
       errors <- getErrorsFrom f
       return (r' ++ errors))
    (return [])
    fs

getErrorsFrom :: FilePath -> IO [String]
getErrorsFrom f = do
  eDecls <- getDeclsFrom f
  case eDecls of
    Right decls -> return (lefts (map buildDecl decls))
    Left e -> return [e]

main :: IO ()
main = putStrLn ("Hello Haskell")
