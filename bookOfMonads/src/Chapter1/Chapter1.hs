{-# LANGUAGE TupleSections #-}

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
