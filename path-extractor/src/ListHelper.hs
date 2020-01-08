module ListHelper where

takeUntil :: [a] -> b -> (a -> b -> Bool) -> [a]
takeUntil [] _ _ = []
takeUntil (x : xs) x' p =
  if p x x' then [] else x : takeUntil xs x' p

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ e []       = e
foldr' f e (x : []) = e
foldr' f e (x : xs) = f x (foldr' f e xs)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ e []       = e
foldl' f e (x : []) = e
foldl' f e (x : xs) = foldl' f (f e x) xs
