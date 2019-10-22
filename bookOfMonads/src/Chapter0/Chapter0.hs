{-# LANGUAGE MultiParamTypeClasses #-}

module Chapter0 where

import Prelude hiding (Eq(..))

class Eq a where
  (==) :: a -> a -> Bool

instance (Eq a, Eq b) => Eq (a, b) where
  (x, y) == (x', y') = x == x' && y == y'

class Container c where
  empty :: c a
  insert :: a -> c a -> c a

instance Container [] where
  empty = []
  insert = (:)

-- data Queue a =
--   Queue
--     { unQueue :: [a]
--     }
newtype Queue a =
  Queue
    { unQueue :: [a]
    }

instance Container Queue where
  empty = Queue []
  insert x (Queue xs) = Queue (xs <> [x])
  -- insert x xs = Queue (unQueue xs <> [x])
