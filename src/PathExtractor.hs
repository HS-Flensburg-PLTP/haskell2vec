module PathExtractor where

import Data.Char
import Data.Maybe (catMaybes)
import LeafCollector

upArrow :: String
upArrow = "+" --[Data.Char.chr 8593] -- ↑

downArrow :: String
downArrow = "-" --[Data.Char.chr 8595] -- ↓

data Path =
  Path [Identifier]
       Identifier
       [Identifier]
       Int
       Int

instance Show Path where
  show (Path lefts root rights len width) =
    foldl (\r n -> show n ++ upArrow ++ r) "" lefts ++
    show root ++ foldl (\r n -> r ++ downArrow ++ show n) "" rights
    -- ++ ";Lenght: " ++ show len ++ ";Width: " ++ show width

data FunctionPaths =
  FunctionPaths String
                [PathContext]

instance Show FunctionPaths where
  show (FunctionPaths s ps) = splitCamel s ++ " " ++ unwords (map show ps)

tokenSeperationChar :: Char
tokenSeperationChar = '|'

splitCamel :: String -> String
splitCamel [] = []
splitCamel (c:cs)
  | isUpper c = tokenSeperationChar : toLower c : splitCamel cs
  | otherwise = c : splitCamel cs

data PathContext =
  PathContext Value
              Path
              Value

instance Show PathContext where
  show (PathContext v1 p v2) = v1 ++ "," ++ show p ++ "," ++ v2

data PathRestrictions =
  Restrictions Int
               Int

defaultRestrictions :: PathRestrictions
defaultRestrictions = (Restrictions 16 4)

extractPaths :: FunctionLeaves -> FunctionPaths
extractPaths = extractPathsWithRestrictions defaultRestrictions

extractPathsWithRestrictions ::
     PathRestrictions -> FunctionLeaves -> FunctionPaths
extractPathsWithRestrictions restrictions (FunctionLeaves name ns) =
  FunctionPaths name pathsContexts
  where
    pathsContexts =
      (catMaybes . map (uncurry (buildPathContext restrictions)) . (buildTuples))
        ns
    --pathsContexts = catMaybes (map (\(l1, l2) -> buildPathContext restrictions l1 l2) (buildTuples ns))

buildTuples :: [a] -> [(a, a)]
buildTuples [] = []
buildTuples (x:xs) = map (\x' -> (x, x')) xs ++ buildTuples xs

buildPathContext :: PathRestrictions -> Leaf -> Leaf -> Maybe PathContext
buildPathContext (Restrictions maxLen maxWidth) (Leaf idents1 is1 len1 v1) (Leaf idents2 is2 len2 v2)
  | abs (len1 - len2) > maxLen = Nothing
  | width > maxWidth = Nothing
  | len > maxLen = Nothing
  | otherwise = Just (PathContext v1 path v2)
  where
    (width, startIndex) = pathInfo 0 is1 is2
    len = len1 + len2 - startIndex * 2 - 1
    leftPath = drop startIndex idents1
    path =
      Path
        (tail leftPath)
        (head leftPath)
        (drop (startIndex + 1) idents2)
        len
        width

pathInfo :: Int -> [Int] -> [Int] -> (Int, Int)
pathInfo index (i:is) (i':is')
  | i == i' = pathInfo (index + 1) is is'
  | otherwise = (i' - i, index)
