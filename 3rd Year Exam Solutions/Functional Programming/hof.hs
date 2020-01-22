-- dooleyb1

-- Higher Order Functions

-- 2017 Q2 a) HOF

hof :: a -> (a -> a -> a)  -> [a] -> a
hof x _ []      = x
hof x f (y:ys)  = f y (hof x f ys)


f1 :: (Num a) => [a] -> a
f1 = hof 42 (*)

f2 :: (Num a) => [a] -> a
f2 = hof 0 g
  where g x y = (99*y)

f3 :: (Num a) => [a] -> a
f3 = hof 0 (+)

f4 :: (Num a) => [a] -> [a]
f4 = hof [] (++)

f5 :: (Num a) => [a] -> a
f5 = hof 0 g
  where g x y = (x-42) + y

f5' [] = 0
f5' (x:xs) = (x-42) + f5' xs

-- 2016 Q2 a) HOF

hof :: a -> (a -> a -> a)  -> [a] -> a
hof x _ []      = x
hof x f (y:ys)  = f y (hof x f ys)


f1 :: (Num a) => [a] -> a
f1 l = hof 1 (*) l

f2 :: (Num a) => [a] -> a
f2 l = hof 0 g l
  where g x y = (1 + y)

f3 :: (Num a) => [a] -> a
f3 l = hof 0 (+) l

f4 :: (Num a) => [a] -> [a]
f4 l = hof [] (++) l

f5 :: (Num a) => [a] -> a
f5 = hof 0 g
  where g x y = (x*x) + y

-- 2015 Q2 a) HOF

hof :: (Num a, Num b) => [a] -> [b] -> (a -> b -> c) -> [c]
hof [] _ _            = []
hof _ [] _            = []
hof (x:xs) (y:ys) f   = f x y : (hof xs ys f)


f1 :: (Num a) => [a] -> [a] -> [a]
f1 xs ys = hof xs ys (*)

f2 :: (Num a) => [a] -> [a] -> [a]
f2 xs ys = hof xs ys (+)

f3 :: (Num a) => [a] -> [a] -> [(a,a)]
f3 xs ys = hof xs ys g
  where g x y = (x, y)

f4 :: (Num a) => [a] -> [a] -> [(a,a)]
f4 xs ys = hof xs ys g
  where g x y = (y, x)

f5 :: (Num a) => [a] -> [a] -> [a]
f5 xs ys = hof xs ys g
  where g x y = x

-- 2014 Q2 a) HOF

hof :: a -> (a -> b -> a) -> [b] -> a
hof z _ [] = z
hof z f (x:xs) = hof (f z x) f xs

f1 :: (Num a) => a -> [a] -> a
f1 p l = hof p f l
  where f x y = x*y

f2 :: (Num a) => a -> [a] -> a
f2 p l = hof p f l
  where f x y = y+1

f3 :: (Num a) => a -> [a] -> a
f3 p l = hof p f l
  where f x y = x+y

f4 :: (Num a) => [a] -> [[a]] -> [a]
f4 p l = hof p f l
  where f x y = x ++ y

f5 :: (Num a) => a -> [a] -> a
f5 p l = hof p f l
  where f x y = (x+y*y)
