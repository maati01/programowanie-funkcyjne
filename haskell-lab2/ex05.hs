countTriangles :: Int -> Int
countTriangles n = length cnt
    where cnt = [(a, b, c) | a <- [1..n], b <- [a..n], c <- [b..n], a ^ 2 + b ^ 2 == c ^ 2]


--isPrime :: Integral t => t -> Bool
--isPrime n = [i | i <- [2..n-1], n `mod` i == 0] == []
--emplementacja nie jest optymalna

--isPrime :: Integral t => t -> Bool
isPrime n = [i | i <- [2..ceiling(sqrt(n))], n `mod` i == 0] == []
