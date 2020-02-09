module HaskellFilesFinder where

import Control.Monad (filterM)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))

getAllFilesFrom :: FilePath -> IO [FilePath]
getAllFilesFrom filePath = do
  allFiles <- listDirectory filePath
  allDirs <- filterM (doesDirectoryExist . (filePath </>)) allFiles
  allFiles <- filterM (doesFileExist . (filePath </>)) allFiles
  foldr
    (\dir r -> do
       r' <- r
       fs <- getAllFilesFrom dir
       return (r' ++ fs))
    (return (getHsFiles (map ((++) (filePath ++ "/")) allFiles)))
    (map ((++) (filePath ++ "/")) allDirs)

getHsFiles :: [FilePath] -> [FilePath]
getHsFiles = filter (\f -> getFileExtension f == hsExt)

hsExt :: FileExtension
hsExt = ".hs"

type FileExtension = String

getFileExtension :: FilePath -> FileExtension
getFileExtension [] = ""
getFileExtension s@(c:cs) =
  if c == '.'
    then s
    else getFileExtension cs
