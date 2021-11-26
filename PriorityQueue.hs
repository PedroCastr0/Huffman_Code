module PriorityQueue where

data Heap a = Empty | Node (Heap (Char, Integer)) (Char, Integer) (Heap (Char, Integer))
    deriving (Show, Eq)

singleton :: (Char, Integer) -> Heap (Char, Integer)
singleton x = insert x Empty

insert :: (Char, Integer) -> Heap (Char, Integer) -> Heap (Char, Integer)
insert x Empty = Node (Empty) x (Empty)
insert x h = mergeHeap h $ singleton x

getMin :: Heap (Char, Integer) -> (Maybe (Char, Integer), Heap (Char, Integer))
getMin Empty = (Nothing, Empty)
getMin (Node l x r) = (Just x, (mergeHeap l r))

mergeHeap :: Heap (Char, Integer) -> Heap (Char, Integer) -> Heap(Char, Integer)
mergeHeap Empty h = h
mergeHeap h Empty = h
mergeHeap hA@(Node esqA xA dirA) hB@(Node esqB xB dirB)
        | snd xA < snd xB = Node (mergeHeap dirA hB) xA esqA
        | otherwise = Node (mergeHeap dirB hA) xB esqB

listToHeap :: [(Char, Integer)] -> Heap (Char, Integer)
listToHeap ls = foldr insert Empty ls

flatten :: Heap(Char, Integer) -> [(Char, Integer)]
flatten node = case getMin node of
            (Nothing, heap) -> []
            (Just val, heap) -> [val] ++ flatten heap
