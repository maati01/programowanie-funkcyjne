{-# LANGUAGE DeriveFunctor #-}

newtype Box a = MkBox a deriving (Show, Functor)

-- instance Functor Box where
--   fmap f (MkBox x) = MkBox (f x)

--
-- data MyList a = EmptyList
--               | Cons a (MyList a) deriving Show

-- instance Functor MyList where
--   fmap _ EmptyList    = EmptyList
--   fmap f (Cons x mxs) = Cons (f x) (fmap f mxs)

--zad1
data MyList a = EmptyList
              | Cons a (MyList a) deriving (Show, Functor)

--zad2 a
data BinTree a = EmptyBT | NodeBT a (BinTree a) (BinTree a) deriving (Show)

instance Functor BinTree where
   fmap _ EmptyBT    = EmptyBT
   fmap f (NodeBT x ml mr) = NodeBT (f x) (fmap f ml) (fmap f mr)

--zad2 b
-- data BinTree a = EmptyBT | NodeBT a (BinTree a) (BinTree a) deriving (Show, Functor)