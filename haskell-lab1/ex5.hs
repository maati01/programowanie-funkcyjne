sgn :: Int -> Int
sgn n = if n < 0
       then -1
       else if n == 0
            then 0
            else 1

absInt :: Int -> Int
absInt x = if x < 0
        then -x
        else x

min2Int :: (Int, Int) -> Int
min2Int (x,y) = if x < y
            then x
            else y

min3Int :: (Int, Int, Int) -> Int
min3Int (x,y,z) = min2Int(x,min2Int(y,z))