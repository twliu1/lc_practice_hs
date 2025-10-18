module ReverseLinkedList where

data ListNode a = Node a (ListNode a) | Empty
  deriving (Show)

reverseLinkedList :: ListNode a -> ListNode a
reverseLinkedList l = go l Empty
  where
    go Empty acc = acc
    go (Node x xs) acc = go xs (Node x acc)
