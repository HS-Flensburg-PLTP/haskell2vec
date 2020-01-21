module Demo where

--mathStuff' 0   = 99
--mathStuff' val = val + 1
--mathStuff' 1   = 2 + 1

--buildPath :: [Identifier] -> [Identifier] -> Path
--buildPath [] rs = Path [] (head rs) (tail rs)
--buildPath ls rs = Path (tail ls) (head ls) (tail rs)

(|<>|) :: Int -> Int -> Bool
i |<>| i2 = i == i2


guards a
  | a == 0 = 5
  | otherwise = 6

--mathStuff :: Int -> Int
--mathStuff val = if val > 0 then 1 else 0

bigMethod val x y
  | y > 0 = case val of
    (Just (x:xs)) -> x * foldr (+) 0 xs
    Nothing -> case x of
      (_,b,v) -> y + y * b - val + head x
  | head (reverse val) < 1 =
    let b = y * y
    in b + z + z
    where z = 5
