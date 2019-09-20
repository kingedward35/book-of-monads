{-# LANGUAGE TupleSections #-}

module Chapter1 where

import Prelude hiding (Functor(..))

-- Chapter 1.1
data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)

numberOfLeaves :: Tree a -> Integer
numberOfLeaves (Leaf _) = 1
numberOfLeaves (Node l r) = numberOfLeaves l + numberOfLeaves r

relabel :: Tree a -> Int -> (Tree (Int, a), Int)
relabel (Leaf x) i = (Leaf (i, x), i + 1)
relabel (Node l r) i =
  let (l', i1) = relabel l i
      (r', i2) = relabel r i1
   in (Node l' r', i2)

-- Node (relabel l i) (relabel r i)
type WithCounter a = Int -> (a, Int)

next :: State s a -> (a -> State s b) -> State s b
f `next` g =
  \i ->
    let (r, i') = f i
     in g r i'

pure' :: a -> State s a
pure' x = \i -> (x, i)

relabel' (Leaf x) = \i -> (Leaf (i, x), i + 1)
relabel' (Node l r) =
  relabel' l `next` \l' -> relabel' r `next` \r' -> pure' (Node l' r')

type State s a = s -> (a, s)

test :: Int -> State Char Int
test i = (i, )

-- Chapter 1.2
length' :: [a] -> Integer
length' = foldr (\x -> (+) 1) 0

concat' :: [a] -> [a] -> [a]
concat' = foldr (:)

concat1 :: [[a]] -> [a]
concat1 = foldr (<>) []

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

singleton :: a -> [a]
singleton x = [x]

-- Chapter 1.3
data Option a
  = None
  | Some a

type Name = String

data Person =
  Person
    { name :: Name
    , age :: Int
    }

validateName :: String -> Option Name
validateName = undefined

validateAge :: Int -> Option Int
validateAge = undefined

validatePerson1 :: String -> Int -> Option Person
validatePerson1 name age =
  case validateName name of
    None -> None
    Some name ->
      case validateAge age of
        None -> None
        Some age -> Some (Person name age)

-- then_ o f = flatten (fmap f o)
then_ :: Option a -> (a -> Option b) -> Option b
then_ v g =
  case v of
    None -> None
    Some v' -> g v'

validatePerson2 :: String -> Int -> Option Person
validatePerson2 name age =
  validateName name `then_` \name' ->
    validateAge age `then_` \age' -> Some (Person name' age')

-- flip' map1 :: Maybe c -> (c -> d) -> Maybe d
map1 :: (a -> b) -> Maybe a -> Maybe b
map1 _ Nothing = Nothing
map1 f (Just a) = Just (f a)

singleton' :: a -> Maybe a
singleton' = Just

flatten :: Maybe (Maybe a) -> Maybe a
flatten (Just (Just x)) = Just x
flatten _ = Nothing

-- flatten oo = then_ oo id
-- flatten oo = \v g -> case v of
--                      Nothing -> Nothing
--                      Just v' -> g v'
-------------------------------------------------------------
-- Chapter 1.4
flatten' :: State s (State s a) -> State s a
flatten' ss = next ss id

-- flatten' ss = \i -> let (r, i') = ss i in id r i'
-- -- in other words
-- flatten' ss = \i -> let (r, i') = ss i in r i'
f :: [a] -> (a -> [b]) -> [b]
f = undefined

-- concatMap'
-- concatMap' xs f = concat (map f xs)
--------------------------------------------------
-- Monad as a context
-- class Monad m where
--   return :: a -> m a
--   (>>=) :: m a -> (a -> m b) -> m b
-- Monad as a box
-- class Monad m where
--   return :: a -> m a
--   fmap :: (a -> b) -> m a -> m b
--   join :: m (m a) -> m a
-- instance Monad Maybe where
--   return = Just
--   (>>=) = then_
---------------------------------------------------
-- Chapter 1.5
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- fmap f fa = fa >>= \a -> return a
class Functor f where
  fmap :: (a -> b) -> f a -> f b
