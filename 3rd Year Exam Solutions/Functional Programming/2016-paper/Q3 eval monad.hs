data Expr = K Int
          | V String
          | Add Expr Expr
          | Dvd Expr Expr
          | Let String Expr Expr

type Dict = [(String, Int)]

fromJust :: Maybe a -> a
fromJust (Just x) = x

ins :: String -> Int -> Dict -> Dict
ins s i d = (s, i):d

lkp :: String -> Dict -> Maybe Int
lkp _ [] = Nothing
lkp x ((k, v):xs)
    | k == x = Just v
    | otherwise = lkp x xs

eval :: (Monad m) => Dict -> Expr -> m Int
eval _ (K i) = return i
eval d (V s) = do
    if i == Nothing then
        fail ("Value not in dict, \"" ++ s ++ "\"")
    else
        return $ fromJust i
    where i = lkp s d
eval d (Add e1 e2) = do
    i1 <- eval d e1
    i2 <- eval d e2
    return $ i1 + i2
eval d (Dvd e1 e2) = do
    i1 <- eval d e1
    i2 <- eval d e2
    if i2 == 0 then
        fail ("Division by zero")
    else
        return $ quot i1 i2
eval d (Let v e1 e2) = do
    i <- eval d e1
    i2 <- eval (ins v i d) e2
    return i2
