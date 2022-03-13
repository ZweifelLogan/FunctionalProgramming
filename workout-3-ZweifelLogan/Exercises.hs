{- These problems are generally expected to require
   pattern-matching and recursion.  See Inclass.hs
   for week3-datatypes-and-recursion (under Files
   on ICON) for related examples.
-}
module Exercises(module Exercises,module BinTree) where
import BinTree

{- return the data stored in the BinTree, using a
   postfix traversal (so the value stored at each
   node appears after values in its subtrees).  We
   did similar examples in class week3; see
   Files/week3-datatypes-and-recursion/Inclass.hs
   on ICON. -}


toListPost :: BinTree a -> [a]
toListPost Leaf = []
toListPost (Node x l r) = toListPost l ++ toListPost r ++ [x]


{- the output tree should be just like the input
   tree, except that the left and right subtrees of
   every node have been switched. -}
mirror :: BinTree a -> BinTree a
mirror Leaf = Leaf
mirror (Node y Leaf Leaf) = sing y
mirror (Node x l r) = Node x (mirror r) (mirror l)

{- implement a function similar to map for lists, but working on BinTrees.
   In more detail: btMap takes in a function from a to b, and should apply
   that function to all the values stored in the input BinTree, to obtain
   the output BinTree. -}
btMap :: (a -> b) -> BinTree a -> BinTree b
btMap f Leaf = Leaf
btMap f (Node x l r) = Node (f x) (btMap f l) (btMap f r)

{- swap the first and second elements of the list, then
   repeat.  When you get to a list of with just one value
   or the empty list, return that list. -}
swap2 :: [a] -> [a]
swap2 [x] = [x]
swap2 [] = []
swap2 (x:y:xs) = y:x:swap2 xs

{- combine the two lists by taking one element from the first,
   then an element from the second, and so forth.  If one list
   ends before the other, return the other. -}
knit :: [a] -> [a] -> [a]
knit x [] = x
knit [] y = y
knit (x:xs) y = x:(knit y xs)

{- return the top part of the tree, cutting off
   at the given Int, which you may assume is greater than or equal to zero -}
btTake :: Int -> BinTree a -> BinTree a
--btTake 0 (Node x l r) = sing x
btTake a Leaf = Leaf
btTake 1 (Node x l r) = sing x
btTake a (Node x l r) = Node x (btTake (a-1) l) (btTake (a-1) r)

{- return all the subtrees you find at the given depth,
   which you may assume is greater than or equal to zero.

   At depth 0, you should return a list containing the input tree.

   For depth greater than 0, if the input tree is a Leaf, return
   the empty list.
-}
btDrop :: Int -> BinTree a -> [BinTree a]
btDrop a Leaf = []
btDrop 0 (Node x l r) = [sing x]
btDrop a (Node x Leaf Leaf) = [Leaf, Leaf]
btDrop a (Node x l r)= (btDrop (a-1) l) ++ (btDrop (a-1) r)

{- this is like zipWith on lists.
   The given function should be applied to corresponding values
   at Nodes.  If one tree has a Node and the other has a Leaf,
   just return Leaf. -}
btZipWith :: (a -> b -> c) -> BinTree a -> BinTree b -> BinTree c
btZipWith f Leaf Leaf = Leaf
btZipWith f (Node x l r) Leaf = Leaf
btZipWith f Leaf (Node y l1 r1) = Leaf
btZipWith f (Node x Leaf Leaf) (Node y Leaf Leaf) = sing (f x y)
btZipWith f (Node x l r) (Node y l1 r1) = Node (f x y) (btZipWith f l l1) (btZipWith f r r1)

{- We can represent paths into a tree as a list of booleans.  Each
   Bool indicates whether we should recurse into the left subtree (True) or
   the right one (False).  Given a path and a tree, return the subtree found
   by following the path.  If the tree ends in a leaf before the path ends,
   then return Nothing. -}
btSubtree :: [Bool] -> BinTree a -> Maybe (BinTree a)
btSubtree a Leaf = Nothing
btSubtree [] (Node x l r) = Just (Node x l r)
btSubtree [True] (Node x l r) = btSubtree [] l
btSubtree [False] (Node x l r) = btSubtree [] r
btSubtree (True:y:as) (Node x l r) = btSubtree (y:as) l
btSubtree (False:y:as) (Node x l r) =  btSubtree (y:as) r

{- btSubst p t1 t2

   Given a path p (as described in the previous problem),
   replace the subtree of t1 that you find following p,
   with t2.  So you are replacing one tree with another,
   inside t1.  If the path is not a valid one into the
   tree (that is, if btSubtree p t1 would return Nothing),
   then just return t1. -}
btSubst :: [Bool] -> BinTree a -> BinTree a -> BinTree a
btSubst [] x y = Leaf
btSubst a x Leaf = x
btSubst a Leaf y = y
btSubst [True] (Node x l r) y = Node x y r
btSubst [False] (Node x l r) y = Node x l y 
btSubst (True:z:as) (Node x l r) y = Node x (btSubst (z:as) l y) r        --btSubst (z:as) l y
btSubst (False:z:as) (Node x l r) y = Node x l (btSubst (z:as) r y) --btSubst (z:as) r y
