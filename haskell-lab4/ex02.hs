-- product type example (one constructor)
type X = Int
type Y = Int

data CartInt2DVec = MkCartInt2DVec X Y -- konwencja: prefix 'Mk' dla konstruktora

xCoord :: CartInt2DVec -> X
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Y
yCoord (MkCartInt2DVec _ y) = y

--
-- data Cart2DVec' a = MkCart2DVec' a a

-- xCoord' :: Cart2DVec' a -> a
-- xCoord' (MkCart2DVec' x _) = x

-- yCoord' :: Cart2DVec' a -> a
-- yCoord' (MkCart2DVec' _ y) = y

--
-- data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}

-- xCoord'' :: Cart2DVec'' a -> a
-- xCoord'' (MkCart2DVec'' {x = xVal, y = _}) = xVal

-- yCoord'' :: Cart2DVec'' a -> a
-- yCoord'' (MkCart2DVec'' {y = yVal, x = _}) = yVal -- uwaga na kolejność x,y

--
-- sum type example (two constructors)
data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL      = error "head': the empty list has no head!"
head' (Cons x xs) = x

--
-- enum type example (special case of sum type)
-- data ThreeColors = Blue |
--                    White |
--                    Red

-- type ActorName = String

-- leadingActor :: ThreeColors -> ActorName
-- leadingActor Blue  = "Juliette Binoche"
-- leadingActor White = "Zbigniew Zamachowski"
-- leadingActor Red   = "Irene Jacob"

--1
data Cart3DVec a = Cart3DVec a a a

xCoord3D :: Cart3DVec a -> a
xCoord3D (Cart3DVec x _ _) = x

yCoord3D :: Cart3DVec a -> a
yCoord3D (Cart3DVec _ y _) = y

zCoord3D :: Cart3DVec a -> a
zCoord3D (Cart3DVec _ _ z) = z

--2
data Cart3DVec' a = Cart3DVec' {x::a, y::a, z::a}

xCoord3D' :: Cart3DVec' a -> a
xCoord3D' (Cart3DVec' {x = xVal, y = _, z = _}) = xVal

yCoord3D' :: Cart3DVec' a -> a
yCoord3D' (Cart3DVec' {y = yVal, x = _, z = _}) = yVal

zCoord3D' :: Cart3DVec' a -> a
zCoord3D' (Cart3DVec' {z = zVal, x = _, y = _}) = zVal

--6
data Shape = Circle Float |
             Rectangle Float Float deriving Show

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle a b) = a * b

--8
data TrafficLights = Red | Yellow | Green

actionFor :: TrafficLights -> String
actionFor Red = "STOP!"
actionFor Yellow = "BE CAREFUL!"
actionFor Green = "GO!"