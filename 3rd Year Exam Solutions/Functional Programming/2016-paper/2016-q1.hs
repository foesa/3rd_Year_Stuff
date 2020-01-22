--1.
--(a)
head :: [a] -> a
head (x:_) = x

--(b)
init :: [a] -> [a]
init (x:[]) = []
init (x:xs) = x:(init xs)

--(c)
last :: [a] -> a
last (x:[]) = x
last (x:xs) = last(xs)

--(d)
span :: (a -> Bool) ->[a] -> ([a],[a])
span bool [] = ([],[])
span bool (x:xs)
   | bool x = let(d,e) = span bool xs in (x:d,e)
   | otherwise = ([],(x:xs))

--(e)
(!!) :: [a] -> Int -> a
(!!) (x:xs) a
    | a == 0 = x
    | otherwise = xs!!(a-1)

--(f)
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f acc []     =  acc
foldl f acc (x:xs) =  foldl f (f acc x) xs

foldl1           :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs)  =  foldl f x xs
foldl1 _ []      =  error "Prelude.foldl1: empty list"
