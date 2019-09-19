{-# LANGUAGE MonadComprehensions #-}

module Chapter2 where

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)

type Name = String

data Person =
  Person
    { name :: Name
    , age :: Int
    }

validateName :: String -> Maybe Name
validateName = undefined

validateAge :: Int -> Maybe Int
validateAge = undefined

-- relabel (Leaf x) = \i -> (Leaf (i, x), i + 1)
-- relabel (Node l r) =
--   relabel l >>= \l' -> relabel r >>= \r' -> return (Node l' r')
validatePerson :: String -> Int -> Maybe Person
validatePerson n a =
  validateName n >>= \name' ->
    validateAge a >>= \age' -> return (Person name' age')

-----------------------------------------------
-- Chapter 2.1.1 Block/do Notation
relabel :: Monad m => Tree a1 -> m (Tree a2)
relabel (Node l r) = do
  l' <- relabel l
  r' <- relabel r
  return (Node l' r')

validatePerson' n a = do
  name' <- validateName n
  age' <- validateAge a
  return (Person name' age')

-- validateAge' age >>= \age' ->
--   let uAge = age < 18 -- or 21
--    in validateName name >>= \name' -> return (Person name' age' uAge)
-- Option (1)
-- do age' <- validateAge age
--   let uAge = age < 18
--   in do name' <- validateName 
--   return (Person name' age' uAge)
-- Option (2)
-- do age' <- validateAge age
--    uAge <- return (age < 18)
--    name' <- validateName name
--    return (Personn name' age' uAge)
-- Option (3)
-- do age' <- validateAge age
--    let uAge = age < 18
--    name' <- validateName name
--    return (Person name' age' uAge)
type State s a = s -> (a, s)

put :: s -> State s ()
put = undefined

get :: State s s
get = undefined

-- incrementCounter :: State Int Int
-- incrementCounter = do
--   n <- get
--   put (n + 1)
--   return (n + 1)
-- (>>) :: Monad m => m a -> m b -> m b
-- m >> n = m >>= \_ -> n -- default definition
-- Chapter 2.1.2 Comprehensions
testMap :: (a -> b) -> [a] -> [b]
testMap _ [] = []
testMap f xs = xs >>= return . f
-- An Example of a Monad Comprehension
-- relabel' (Node l r) = [Node l' r' | l' <- relabel' l, r' <- relabel' r]
-- validatePerson name age = [Person name' age' | name' <- validateName name, age' <- validateAge age]
----------------------------------------------
-- Chapter 2.1.3 Bangs in Idris
-- relabel (Node l r) = return (Node !(relabel l) !(relabel r))
-- validatePerson name age = return (Person !(validateName name) !(validateAge age)) 
