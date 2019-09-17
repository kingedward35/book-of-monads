instance (Eq a, Eq b) => Eq (a, b) where
  a == b = True
  _ == _ = False

class Container c where
  empty :: c a
  insert :: a -> c a -> ca

instance Container [] where
  empty = []
  insert x xs = x : xs
