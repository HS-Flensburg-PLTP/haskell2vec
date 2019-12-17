module Demo where

id' :: a -> a
id' a = a

mathStuff :: Int -> Int
mathStuff val = if val > 0 then val * val else val + 1
