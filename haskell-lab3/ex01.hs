--1
f1 :: (Num a) => a -> a
f1 = \x -> x - 2 

f2 :: (Floating a) => a -> a -> a
f2 = \x y -> sqrt(x^2 + y^2)

f3 :: (Integral a, Floating b) => a -> a -> a -> b
f3 = \x y z -> sqrt(fromIntegral(x^2 + y^2 + z^2)) 

--2
f4 = \x -> x*2
f5 = \x -> 2*x
f6 = \x -> 2^x
f7 = \x -> x^2
f8 = \x -> 2/x
f9 = \x -> x/3
f10 = \x -> 4-x

--3
f11 = \x ->  case () of
    _ | x `mod` 2 == 0 -> True
      | otherwise -> False

--ewentualnie f11 = \x -> if x `mod` 2 == 0 then True else False

f12 = \x -> let y = sqrt x in 2 * y ^ 3 * (y + 1)

f13 = \x -> if x == 1 then 3 else 0