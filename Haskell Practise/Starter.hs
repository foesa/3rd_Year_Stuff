main :: IO ()
main = putStrLn "Hello World"

tailer :: [t] -> [t]
tailer [] = error "empty list"
tailer (x:xs) = xs

newHead :: [a] -> a
newHead [] = error "empty list"
newHead (x:xs) = x

ender :: [t] -> t
ender [] = error "empty list"
ender [x] = x
ender (_:xs) = ender(xs)

adder :: [t] -> [t] -> [t]
adder [] [] = error "both empty lists"
adder [] y = y
adder [x] y = x:y
adder (x:xs) y = x : (adder xs y)

seclast :: [t] -> [t]
seclast [] = error "both empty lists"
seclast (x:y:[]) = [x]
seclast (x:xs) = x : seclast xs

reverser :: [t] -> [t]
reverser xs = reverser xs []
  where
    reverser []  acc = acc
    reverser (x:xs) acc = reverser xs (x:acc)

breaker :: (a -> Bool) -> [a] -> ([a],[a])
breaker _ xs@[] = (xs,xs)
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

laster :: [a] -> a
laster [] = error "empty list"
laster [a] = a
laster (x:xs) = laster xs

splitAter :: Int -> [a] -> ([a],[a])
splitAter 0 y =  ([],y)
splitAter x (y:ys) = (y:ys',ys'')
  where
    (ys',ys'') = splitAter (x-1) ys


lengther :: [a] ->  Int
lengther [] = 0
lengther (x:xs) = lengther xs + 1

replicatew :: Int -> a -> [a]
replicatew 0 x = []
replicatew x y = y: replicatew (x-1) y

foldish :: (a-> a -> a) -> [a] -> a
foldish _ xs@[] = error "empty list"
foldish f xs@(x:y:[]) = f x y
foldish f xs@(x:y:ys) = foldish (f) (f x y:ys)

indexer :: [a]-> Int -> a
indexer [] _ = error "list is emptyy"
indexer (x:xs) 0 = x
indexer yer@(x:xs) n = if lengther yer > n then indexer xs (n-1) else error "List is too small"

unzipper :: [(a,b)] -> ([a],[b])
unzipper [] = ([],[])
unzipper ((x,y):xs) = (x:xs',y:ys')
  where
    (xs', ys') = unzipper xs

zipper :: [a] -> [b] -> [(a,b)]
zipper (x:xs) (y:ys) = (x,y):zipper xs ys
zipper _ _ = []

f2 :: a -> (a->a->a) -> [a] -> a
f2 x fx [] = x
f2 x fx (y:ys) = fx y (f2 x fx ys)

f2r = f2 1 f2x
  where
    f2x x y = (99*y)
f1 = f2  1 (*)
