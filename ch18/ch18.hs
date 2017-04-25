{-# LANGUAGE InstanceSigs #-}

import Prelude hiding (Left, Right)
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

bind :: Monad m => (a -> m b) -> m a -> m b
bind = undefined

-- Either

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap :: (b -> c) -> Sum a b -> Sum a c
  fmap = undefined

instance Applicative (Sum a) where
  pure :: b -> Sum a b
  pure = undefined

  (<*>) :: Sum a (b -> c) -> Sum a b -> Sum a c
  (<*>) = undefined

instance Monad (Sum a) where
  return :: b -> Sum a b
  return = undefined

  (>>=) :: Sum a b -> (b -> Sum a c) -> Sum a c
  (>>=) = undefined

-- Nope

data Nope a = NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap :: (a -> b) -> Nope a -> Nope b
  fmap = undefined

instance Applicative Nope where
  pure :: a -> Nope a
  pure = undefined

  (<*>) :: Nope (a -> b) -> Nope a -> Nope b
  (<*>) = undefined

instance Monad Nope where
  return :: a -> Nope a
  return = undefined

  (>>=) :: Nope a -> (a -> Nope b) -> Nope b
  (>>=) = undefined

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where (=-=) = eq

nopeTrigger = undefined :: Nope (Int, String, Int)

nopeTest = quickBatch $ monad nopeTrigger

-- PhhhbbtttEither

data PhhhbbtttEither b a =
    Left a
  | Right b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap :: (a -> c) -> PhhhbbtttEither b a -> PhhhbbtttEither b c
  fmap = undefined

instance Applicative (PhhhbbtttEither b) where
  pure :: a -> PhhhbbtttEither b a
  pure = undefined
  
  (<*>) :: PhhhbbtttEither b (a -> c) -> PhhhbbtttEither b a -> PhhhbbtttEither b c
  (<*>) = undefined

instance Monad (PhhhbbtttEither b) where
  return :: a -> PhhhbbtttEither b a
  return = undefined

  (>>=) :: PhhhbbtttEither b a -> (a -> PhhhbbtttEither b c) -> PhhhbbtttEither b c
  (>>=) = undefined

instance (Arbitrary b, Arbitrary a) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(3, return $ Left a),
               (1, return $ Right b)]

instance (Eq b, Eq a) => EqProp (PhhhbbtttEither b a) where (=-=) = eq

phhhbbtttEitherTrigger = undefined :: PhhhbbtttEither (Int, String, Int) (Int, String, Int)
phhhbbtttEitherTest = quickBatch $ monad phhhbbtttEitherTrigger

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

instance Monad Identity where
  return :: a -> Identity a
  return = undefined

  (>>=) :: Identity a -> (a -> Identity b) -> Identity b
  (>>=) = undefined

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq

idTrigger = undefined :: Identity (Int, String, Int)

idTest = quickBatch $ monad idTrigger

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

instance Monad List where
  return :: a -> List a
  return = undefined

  (>>=) :: List a -> (a -> List b) -> List b
  (>>=) = undefined

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return Nil),
               (3, return $ Cons a b)]

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

instance Eq a => EqProp (List a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = take' 3000 xs
          ys' = take' 3000 ys

listTrigger = undefined :: List (Int, String, Int)

listTest = quickBatch $ monad listTrigger

-- Other Exercises

j :: Monad m => m (m a) -> m a
j = undefined

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = undefined

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = undefined

a :: Monad m => m a -> m (a -> b) -> m b
a = undefined

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh = undefined

flipType :: Monad m => [m a] -> m [a]
flipType = undefined
