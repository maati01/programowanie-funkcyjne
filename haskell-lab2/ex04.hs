import Data.Char
isPalindrome :: [Char] -> Bool
isPalindrome s =
    if s == reverse(s)
        then True
        else False

getElemAtIdx l i = last(take (i + 1) l) -- l - list, i - index


capitalize :: [Char] -> [Char]
capitalize w = [toUpper(head w)] ++ tail w  -- capitalize "ala" = "Ala"