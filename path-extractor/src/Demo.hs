module Demo where

--mathStuff' 0   = 99
--mathStuff' val = val + 1
--mathStuff' 1   = 2 + 1

--buildPath :: [Identifier] -> [Identifier] -> Path
--buildPath [] rs = Path [] (head rs) (tail rs)
--buildPath ls rs = Path (tail ls) (head ls) (tail rs)

--const' :: a -> b -> a
--const' a b = a

letter val =
  let val' = val * val
  in val' - val

--(|<>|) :: Int -> Int -> Bool
--i |<>| i2 = i == i2

guards a
    | a == 0 = 5
    | otherwise = 6

--mathStuff :: Int -> Int
--mathStuff val = if val > 0 then 1 else 0
