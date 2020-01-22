-- Given binary search tree
data Tree = Empty
  | Single Int String
  | Many Tree Int String Tree

-- Search function
search :: Int -> Tree -> String

search x (Single i s)
  | x == i = s

search x (Many left i s right)
  | x == i = s
  | x > i  = search x right
  | x < i  = search x left

-- Describe two ways in which the search function can fail
--
--  1) Search on empty
--  2) Search on non-existing string

-- Add in error handling (Maybe)
search :: Int -> Tree -> Maybe String

search x (Empty) = Nothing

search x (Single i s)
  | x == i = Just s
  | otherwise = Nothing

search x (Many left i s right)
  | x == i = Just s
  | x > i  = search x right
  | x < i  = search x left

-- Add in error handling (Monads)
search :: (Monad m) => Int -> Tree -> m String

search x (Single i s)
  | x == i = return s
  | otherwise = fail "Key not found"

search x (Many left i s right)
  | x == i = return s
  | x > i  = search x right
  | x < i  = search x left
  | otherwise = fail "Search failure"
