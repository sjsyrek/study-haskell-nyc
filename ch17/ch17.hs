{-# LANGUAGE InstanceSigs #-}

import Data.Monoid
import Control.Applicative hiding (ZipList)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Identity

newtype Identity a = Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap :: (a -> b) -> Identity a -> Identity b
  fmap = undefined

instance Applicative Identity where
  pure :: a -> Identity a
  pure = undefined
  
  (<*>) :: Identity (a -> b) -> Identity a -> Identity b
  (<*>) = undefined

-- Constant

newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Show)

instance Functor (Constant a) where
  fmap :: (b -> c) -> Constant a b -> Constant a c
  fmap = undefined

instance Monoid m => Applicative (Constant m) where
  pure :: b -> Constant m b
  pure = undefined

  (<*>) :: Constant m (a -> b) -> Constant m a -> Constant m b
  (<*>) = undefined

-- List

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Monoid (List a) where
  mempty :: List a
  mempty = undefined

  mappend :: List a -> List a -> List a
  mappend = undefined

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap = undefined

instance Applicative List where
  pure :: a -> List a
  pure = undefined

  (<*>) :: List (a -> b) -> List a -> List b
  (<*>) = undefined

-- ZipList

newtype ZipList a = ZipList (List a)
  deriving (Eq, Show)

instance Monoid a => Monoid (ZipList a) where
  mempty :: ZipList a
  mempty = undefined

  mappend :: ZipList a -> ZipList a -> ZipList a
  mappend = undefined

instance Functor ZipList where
  fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap = undefined

instance Applicative ZipList where
  pure :: a -> ZipList a
  pure = undefined

  (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
  (<*>) = undefined

-- Validation

data Validation e a =
    Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap :: (a -> b) -> Validation e a -> Validation e b
  fmap = undefined

instance Monoid e => Applicative (Validation e) where
  pure ::  a -> Validation e a
  pure = undefined

  (<*>) :: Validation e (a -> b) -> Validation e a -> Validation e b
  (<*>) = undefined

-- Pair

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap :: (a -> b) -> Pair a -> Pair b
  fmap = undefined

instance Applicative Pair where
  pure :: a -> Pair a
  pure = undefined

  (<*>) :: Pair (a -> b) -> Pair a -> Pair b
  (<*>) = undefined

-- Two

data Two a b = Two a b deriving (Eq, Show)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty :: Two a b
  mempty = undefined

  mappend :: Two a b -> Two a b -> Two a b
  mappend = undefined

instance Functor (Two a) where
  fmap :: (b -> c) -> Two a b -> Two a c
  fmap = undefined

instance Monoid a => Applicative (Two a) where
  pure :: b -> Two a b
  pure = undefined

  (<*>) :: Two a (b -> c) -> Two a b -> Two a c
  (<*>) = undefined

-- Three

data Three a b c = Three a b c deriving (Eq, Show)

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
  mempty :: Three a b c
  mempty = Three mempty mempty mempty

  mappend :: Three a b c -> Three a b c -> Three a b c
  mappend = undefined

instance Functor (Three a b) where
  fmap :: (c -> d) -> Three a b c -> Three a b d
  fmap = undefined

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure :: c -> Three a b c
  pure = undefined

  (<*>) :: Three a b (c -> d) -> Three a b c -> Three a b d
  (<*>) = undefined

-- Three'

data Three' a b = Three' a b b deriving (Eq, Show)

instance (Monoid a, Monoid b) => Monoid (Three' a b) where
  mempty :: Three' a b
  mempty = Three' mempty mempty mempty

  mappend :: Three' a b -> Three' a b -> Three' a b
  mappend = undefined

instance Functor (Three' a) where
  fmap :: (b -> c) -> Three' a b -> Three' a c
  fmap = undefined

instance Monoid a => Applicative (Three' a) where
  pure :: b -> Three' a b
  pure = undefined

  (<*>) :: Three' a (b -> c) -> Three' a b -> Three' a c
  (<*>) = undefined

-- Four

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
  mempty :: Four a b c d
  mempty = undefined

  mappend :: Four a b c d -> Four a b c d -> Four a b c d
  mappend = undefined

instance Functor (Four a b c) where
  fmap :: (d -> e) -> Four a b c d -> Four a b c e
  fmap = undefined

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure :: d -> Four a b c d
  pure = undefined

  (<*>) :: Four a b c (d -> e) -> Four a b c d -> Four a b c e
  (<*>) = undefined

-- Four'

data Four' a b = Four' a a a b deriving (Eq, Show)

instance (Monoid a, Monoid b) => Monoid (Four' a b) where
  mempty :: Four' a b
  mempty = undefined

  mappend :: Four' a b -> Four' a b -> Four' a b
  mappend = undefined

instance Functor (Four' a) where
  fmap :: (b -> c) -> Four' a b -> Four' a c
  fmap = undefined

instance Monoid a => Applicative (Four' a) where
  pure :: b -> Four' a b
  pure = undefined

  (<*>) :: Four' a (b -> c) -> Four' a b -> Four' a c
  (<*>) = undefined

-- Tests

type SSI = (String, String, Sum Int)

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' 0 xs = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return Nil),
               (2, return $ Cons a (Cons b Nil)),
               (3, return $ Cons a Nil)]

instance Eq a => EqProp (List a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let l = xs
                in take' 3000 l
          ys' = let l = ys
                in take' 3000 l

instance Arbitrary a => Arbitrary (ZipList a) where
  arbitrary = do
    l <- arbitrary
    return $ ZipList l

instance Eq a => EqProp (ZipList a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList l) = xs
                in take' 3000 l
          ys' = let (ZipList l) = ys
                in take' 3000 l

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a  <- arbitrary
    a' <- arbitrary
    return $ Pair a a'

instance Eq a => EqProp (Pair a) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (Eq a, Eq b) => EqProp (Two a b) where (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Three' a b b

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Four' a a a b

instance (Eq a, Eq b) => EqProp (Four' a b) where (=-=) = eq

listTrigger = undefined :: List (String, Int, Int)
zipListTrigger = undefined :: ZipList (String, Int, Int)
pairTrigger = undefined :: Pair (String, String, Int)
twoTrigger = undefined :: Two SSI SSI
threeTrigger = undefined :: Three SSI SSI SSI
threeTrigger' = undefined :: Three' SSI SSI
fourTrigger = undefined :: Four SSI SSI SSI SSI
fourTrigger' = undefined :: Four' SSI SSI

testList = quickBatch $ applicative listTrigger
testZipList = quickBatch $ applicative zipListTrigger
testPair = quickBatch $ applicative pairTrigger
testTwo = quickBatch $ applicative twoTrigger
testThree = quickBatch $ applicative threeTrigger
testThree' = quickBatch $ applicative threeTrigger'
testFour = quickBatch $ applicative fourTrigger
testFour' = quickBatch $ applicative fourTrigger'
