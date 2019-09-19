{-# LANGUAGE MultiParamTypeClasses #-}

module Chapter0 where

instance (Eq a, Eq b) => Eq (a, b) where
  a == b = True
  _ == _ = False

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
