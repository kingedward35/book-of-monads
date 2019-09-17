instance (Eq a, Eq b) => Eq (a, b) where
  a == b = True
  _ == _ = False
