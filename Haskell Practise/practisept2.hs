tailer :: [a] -> [a]
tailer [] = error "Empty String"
tailer (x:xs) = xs

concer :: [a] -> [a] -> [a]
concer [] [] = []
concer [] x = x
concer [x] y = x:y
concer (x:xs) y = x : concer(xs y)

initer :: [a] -> [a]
initer [] = error "empty list"
initer [x] = []
initer (x:xs) = x:(initer xs)

reverser :: [a] -> [a]
reverser [] = error "empty list"
reverser [x] = [x]
reverser (x:xs) = reverser(xs) : x

breaker :: (a -> Bool) -> [a] -> ([a],[a])
breaker _ xs@[] = (xs:xs)
breaker p xs@(x:y)
  | p x = ([],xs)
  | otherwise = let (ys,zs) = breaker p y in (x:ys,zs)

maximumer :: (Ord a,Num a) => [a] -> a
maximumer [] = error "empty string"
maximumer [x] = x
maximumer (x:xs)
  | a > b = a
  | a < b = b
  | otherwise = a
  where a = x
        b = maximumer xs

splitAter :: Int -> [a] -> ([a],[a])
splitAter 0 y = ([],y)
splitAter n (y:ys) = (y:ys',ys'')
  where
    (ys',ys'') = splitAter(n-1 ys)

replicater :: Int -> a -> [a]
replicater 1 v = [v]
replicater n v = (v :replicater n-1 v)

foldish :: (a->a->a) -> [a] -> a
foldish f [] = error "empty list"
foldish f xs@(x:y:[]) = f x y
foldish f (x:y:ys) = foldish (f) (f x y:ys)
