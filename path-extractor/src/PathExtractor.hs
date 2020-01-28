module PathExtractor where
import           AST_Builder
import           Data.Char
import           Data.Maybe  (catMaybes)

{-
data PathPartConnector =
  Up | Down

instance Show PathPartConnector where
  show Up = " U "-- ↑
  show Down = " D "-- ↓
-}

upArrow :: String
upArrow = "|"--" U "-- ↑

downArrow :: String
downArrow = "|"--" D "-- ↓

data Path =
  Path [Identifier] Identifier [Identifier] Int Int

instance Show Path where
  show (Path lefts root rights len width) = --show lefts ++ show root ++ show rights
    foldl (\r n -> show n ++ upArrow ++ r) "" lefts ++ show root ++
    foldl (\r n -> r ++ downArrow ++ show n ) "" rights-- ++ ";Lenght: " ++ show len ++ ";Width: " ++ show width

data FunctionPath =
  FunctionPath String [PathContext]

instance Show FunctionPath where
  show (FunctionPath s ps) = splitCamel s ++ " " ++ unwords (map show ps)

tokenSeperationChar :: Char
tokenSeperationChar = '|'

splitCamel :: String -> String
splitCamel [] = []
splitCamel (c : cs)
  | isUpper c = tokenSeperationChar : toLower c : splitCamel cs
  | otherwise = c : splitCamel cs

data PathContext =
  PathContext Value Path Value

instance Show PathContext where
  show (PathContext v1 p v2) =
    v1 ++ "," ++ show p ++ "," ++ v2


data Options =
  Options Int Int Bool
  --width length includeSemiPath

defaultOptions :: Options
defaultOptions = (Options 4 16 False)


extractPaths :: FunctionNodes -> FunctionPath
extractPaths = extractPathWithOptions defaultOptions

extractPathWithOptions :: Options -> FunctionNodes -> FunctionPath
extractPathWithOptions options (FunctionNodes name nss) = FunctionPath name (concatMap (rep options) nss)

rep :: Options -> [Node] -> [PathContext]
rep options = catMaybes . map (uncurry (buildPath options)) . buildTuples

buildTuples :: [a] -> [(a,a)]
buildTuples []       = []
buildTuples (x : xs) = map (\x' -> (x,x')) xs ++ buildTuples xs

buildPath :: Options -> Node -> Node -> Maybe PathContext
buildPath (Options maxWidth maxLen _) l1@(Leaf idents1 is1 len1 v1) l2@(Leaf idents2 is2 len2 v2)
  | abs (len1 - len2) > maxLen = Nothing
  | width > maxWidth = Nothing
  | len > maxLen = Nothing
  | otherwise =
    Just (PathContext v1 path v2)
  where
    (width, parentDepth) = pathInfo 0 is1 is2
    len = len1 + len2 - parentDepth * 2
    leftPath = drop parentDepth idents1
    path = Path (tail leftPath) (head leftPath) (drop (parentDepth + 1) idents2) len width


pathInfo :: Int -> [Int] -> [Int] -> (Int, Int)
pathInfo depth (i : is) (i' : is')
  | i == i'   = pathInfo (depth + 1) is is'
  | otherwise = (i' - i, depth)
