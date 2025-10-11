module AddTwoNumbers where

data ListNode = Node Int ListNode | Empty
  deriving (Show)

addTwoNumbers :: ListNode -> ListNode -> ListNode
addTwoNumbers l1 l2 = go l1 l2 0
  where
    go Empty Empty carry = if carry == 0 then Empty else Node carry Empty
    go (Node x xs) Empty carry = let (d, c) = (x + carry) `divMod` 10 in Node d (go xs Empty c)
    go Empty (Node y ys) carry = let (d, c) = (y + carry) `divMod` 10 in Node d (go Empty ys c)
    go (Node x xs) (Node y ys) carry =
      let (d, c) = (x + y + carry) `divMod` 10 in
      Node d (go xs ys c)
