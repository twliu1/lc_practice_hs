module MergeTwoSortedLists where

data ListNode = Node Int ListNode | Empty
  deriving (Show, Eq)

mergeTwoLists :: ListNode -> ListNode -> ListNode
mergeTwoLists Empty l2 = l2
mergeTwoLists l1 Empty = l1
mergeTwoLists (Node x next1) (Node y next2)
  | x <= y    = Node x $ mergeTwoLists next1 (Node y next2)
  | otherwise = Node y $ mergeTwoLists (Node x next1) next2

-- Helper for testing
toListNode :: [Int] -> ListNode
toListNode [] = Empty
toListNode (x:xs) = Node x (toListNode xs)
