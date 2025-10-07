module ValidParentheses where

isValid :: String -> Bool
isValid s = check s []
  where
    check :: String -> [Char] -> Bool
    check [] stack = null stack
    check (c:cs) stack
      | isOpening c = check cs (c:stack)
      | isClosing c = not (null stack) && matches (head stack) c && check cs (tail stack)
      | otherwise   = check cs stack

    isOpening :: Char -> Bool
    isOpening c = c `elem` ['(', '{', '[']

    isClosing :: Char -> Bool
    isClosing c = c `elem` [')', '}', ']']

    matches :: Char -> Char -> Bool
    matches '(' ')' = True
    matches '{' '}' = True
    matches '[' ']' = True
    matches _ _     = False