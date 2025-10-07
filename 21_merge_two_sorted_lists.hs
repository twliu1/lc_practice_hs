module MergeTwoSortedLists where

data Node = Node Int (Maybe Node) | Empty
  deriving (Show, Eq)

mergeTwoLists :: Maybe Node -> Maybe Node -> Maybe Node
mergeTwoLists Nothing l2 = l2
mergeTwoLists l1 Nothing = l1
mergeTwoLists (Just (Node x next1)) (Just (Node y next2))
  | x <= y    = Just (Node x (mergeTwoLists next1 (Just (Node y next2))))
  | otherwise = Just (Node y (mergeTwoLists (Just (Node x next1)) next2))

-- Helper for testing
toNode :: [Int] -> Maybe Node
toNode [] = Nothing
toNode (x:xs) = Just (Node x (toNode xs))
