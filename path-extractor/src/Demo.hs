module Demo where



mathStuff' 0 = 99
mathStuff' val = val + 1

--const' :: a -> b -> a
const' a b = a

mathStuff :: Int -> Int
mathStuff val = if val > 0 then val * val else val + 1
