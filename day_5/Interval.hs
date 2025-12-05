module Interval (
    Interval, intervalContains,
    intervalIntersects, intervalSize,

    empty, add,
    contains, overlaps
) where

-- Inclusive range
type Interval = (Int, Int)
data IntervalNode = Node IntervalNode (Interval, Int) IntervalNode | Nil deriving Show

intervalContains :: Interval -> Int -> Bool
intervalContains (lo, hi) x = x >= lo && x <= hi

intervalIntersects :: Interval -> Interval -> Bool
intervalIntersects (lo1, hi1) (lo2, hi2) = lo1 <= hi2 && hi1 >= lo2 

intervalSize :: Interval -> Int
intervalSize (lo, hi) = hi - lo + 1

empty :: IntervalNode
empty = Nil

add' :: IntervalNode -> IntervalNode -> IntervalNode
add' x Nil = x
add' Nil x = x
add' root@(Node l (range1, maxInTree1) r) x@(Node _ (range2, maxInTree2) _) =
    let maxInTree' = max maxInTree1 maxInTree2
    in case compare range2 range1 of
        LT -> Node (add' l x) (range1, maxInTree') r
        GT -> Node l (range1, maxInTree') (add' r x)
        EQ -> root

add :: IntervalNode -> Interval -> IntervalNode
add n r@(_, hi) = add' n (Node Nil (r, hi) Nil)

contains :: IntervalNode -> Int -> Bool
contains Nil _ = False
contains (Node l (range@(lo,hi), maxInTree) r) x
    | x > maxInTree = False
    | x == maxInTree = True
    | intervalContains range x = True 
    | otherwise = isInRight || isInLeft
        where
        isInRight = x >= lo && contains r x
        isInLeft = contains l x

overlaps :: IntervalNode -> Interval -> [Interval]
overlaps Nil _ = []
overlaps (Node l (range1@(lo1, hi1), maxInTree) r ) range2@(lo2, hi2) = 
    -- I have no idea what I am doing
    if intervalIntersects range2 range1
    then range1 : range2 : overlaps l range2 ++ overlaps r range2
    else overlaps l range2 ++ overlaps r range2
    
