-- Q1 a) Return the tail of a list
tail' :: [a] -> [a]
tail' [] = error "empty list"
tail' (x:xs) = xs

-- Q1 b) Concatenate two lists together
concat' :: [a] -> [a] -> [a]
concat' [] [] = []
concat' [] ys = ys
concat' (x:xs) ys = x : (concat' xs ys)

-- Q1 c) Return everything but the last element of the list
init' :: [a] -> [a]
init' [] = error "empty list"
init' (x:y:[]) = x:[]
init' (x:xs) = x : (init' xs)

-- Q1 c) Other implementation
init [x]                =  []
init (x:xs)             =  x : init xs
init []                 =  errorEmptyList "init"

-- Q1 c) Other implementation
init []                 =  errorEmptyList "init"
init (x:xs)             =  init' x xs
  where init' _ []     = []
        init' y (z:zs) = y : init' z zs

-- Q1 d) Reverse a list argument (uses an accumulator)
reverse' :: [a] -> [a]
reverse' xs = reverse' xs []
  where
    reverse' []     acc = acc
    reverse' (x:xs) acc = reverse' xs (x:acc)


last' :: [a] -> a
last' [] = error "empty list"
last' (x:[]) = x
last' (x:xs) = last' xs

-- Q1 e) Break which takes a predicate and a list and splits the list
--       into two lists, the first list being the longest prefix that
--       does not satisfy the predicate
break' :: (a -> Bool) -> [a] -> ([a], [a])
break' _ xs@[]           =  (xs, xs)
break' p xs@(x:xs')
           | p x         =  ([],xs)                                   -- If head satisfies predicate, return empty list with the rest
           | otherwise   =  let (ys,zs) = break' p xs' in (x:ys,zs)   -- Call break' with the tail

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = [x | x <- xs, f x]

-- Q1 f) Compute the maximum of a non empty list
maximum' :: Ord a => [a] -> a
maximum' [] = error "max of empty list"
maximum' [x] = x
maximum' (x:xs) = max' x (maximum' xs)

max' :: Ord a => a -> a -> a
max' a b
  | a > b   = a
  | a < b   = b
  | a == b  = a
