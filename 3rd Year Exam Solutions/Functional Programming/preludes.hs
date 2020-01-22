-- dooleyb1

-- Most have been tested on GHCI, may not be the optimised/best solution

-- 2018 Q1 a) Return the tail of a list
tail' :: [a] -> [a]
tail' [] = error "empty list"
tail' (x:xs) = xs

-- 2018 Q1 b) Concatenate two lists together
concat' :: [a] -> [a] -> [a]
concat' [] [] = []
concat' [] ys = ys
concat' (x:xs) ys = x : (concat' xs ys)

-- 2018 Q1 c) Return everything but the last element of the list
init' [x]                =  []
init' (x:xs)             =  x : init xs
init' []                 =  errorEmptyList "init"

-- 2018 Q1 d) Reverse a list argument (uses an accumulator)
reverse' :: [a] -> [a]
reverse' xs = reverse' xs []
  where
    reverse' []     acc = acc
    reverse' (x:xs) acc = reverse' xs (x:acc)

-- 2018 Q1 d) Return the last element of a list
last' :: [a] -> a
last' [] = error "empty list"
last' (x:[]) = x
last' (x:xs) = last' xs

-- 2018 Q1 e) Break which takes a predicate and a list and splits the list
--       into two lists, the first list being the longest prefix that
--       does not satisfy the predicate
break' :: (a -> Bool) -> [a] -> ([a], [a])
break' _ xs@[]           =  (xs, xs)
break' p xs@(x:xs')
           | p x         =  ([],xs)                                   -- If head satisfies predicate, return empty list with the rest
           | otherwise   =  let (ys,zs) = break' p xs' in (x:ys,zs)   -- Call break' with the tail

-- 2018 Q1 f) Compute the maximum of a non empty list
maximum' :: Ord a => [a] -> a
maximum' [] = error "max of empty list"
maximum' [x] = x
maximum' (x:xs) = max' x (maximum' xs)

max' :: Ord a => a -> a -> a
max' a b
 | a > b   = a
 | a < b   = b
 | a == b  = a

-- 2017 Q1 d) Split a list at a given int
splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs
  | n <= 0 = ([], xs)
  | otherwise          = splitAt' n xs
    where
        splitAt' :: Int -> [a] -> ([a], [a])
        splitAt' _  []     = ([], [])
        splitAt' 1  (x:xs) = ([x], xs)
        splitAt' m  (x:xs) = (x:xs', xs'')
          where
            (xs', xs'') = splitAt' (m - 1) xs

-- 2017 Q1 e) Replicate x n times
replicate' :: Int -> a -> [a]
replicate' 0 _  = []
replicate' n x  = x : replicate (n-1) x

-- 2017 Q1 f)
foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' f (x:xs) = foldl' f x xs
foldl1' _ []     =  errorEmptyList "foldl1"

foldl' :: (a -> a -> a) -> a -> [a] -> a
foldl' _ z []     = z
foldl' f z (x:xs) = foldl' f (f z x) xs

-- 2016 Q1 a) Head of a list
head' :: [a] -> a
head' []     = errorEmptyList "head'"
head' (x:xs) = x

-- 2016 Q1 d) Span over a list using a predicate
span' :: (a -> Bool) -> [a] -> ([a], [a])
span' _ xs@[]            =  (xs, xs)
span' p xs@(x:xs')
         | p x          =  let (ys,zs) = span p xs' in (x:ys,zs)
         | otherwise    =  ([],xs)

-- 2016 Q1 e) Return element at index n
(!!) :: [a] -> Int -> a
xs !! n | n < 0 = error "negative index"
[] !! _         = error "(!!)"
(x:_) !! 0      = x
(x:xs) !! n     = xs !! (n-1)

-- 2015 Q1 a) Repeat an element infitely
repeat' :: a -> [a]
repeat' x = x : repeat' x

-- 2015 Q1 c) Concat a list of lists to one list
concat' :: [[a]] -> [a]
concat' = foldr (++) []

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z []     =  z
foldr f z (x:xs) =  f x (foldr f z xs)

-- 2015 Q1 d) Zip two seperate lists
zip :: [a] -> [b] -> [(a,b)]
zip (a:as) (b:bs) = (a,b) : zip as bs
zip _       _     = []

-- 2015 Q1 e) Unzip a tuple
unzip' :: [(a,b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' ((a,b):xs) = (a: ???, b: ???)
  where ??? = unzip' xs

-- 2015 Q1 f) Minimum
minimum' :: Ord a => [a] -> a
minimum' [] = error "min of empty list"
minimum' [x] = x
minimum' (x:xs) = min' x (minimum' xs)

min' :: Ord a => a -> a -> a
min' a b
  | a < b   = a
  | a > b   = b
  | a == b  = a

-- 2013 Q1 a) Null
null :: [a] -> Bool
null [] = True
null _ = False

-- 2013 Q1 c) Drop while
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ xs@[]       =  []
dropWhile' p xs@(x:xs')
           | p x         =  xs
           | otherwise   =  dropWhile' p xs'

-- 2013 Q1 e) Filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = [x | x <- xs, f x]

-- 2013 Q1 f) Foldr1
foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' f (x:xs) = foldr' f x xs
foldr1' _ []     =  errorEmptyList "foldr1"

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ z []     =  z
foldr' f z (x:xs) =  f x (foldr' f z xs)
