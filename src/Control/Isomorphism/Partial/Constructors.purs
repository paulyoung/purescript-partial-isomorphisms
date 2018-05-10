module Control.Isomorphism.Partial.Constructors where

import Prelude hiding (unit)

import Control.Isomorphism.Partial (Iso(..))
import Data.Either (Either(..), hush)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Unit as Unit

nil :: forall a. Iso Unit (List a)
nil = Iso f g
  where
  f x
    | x == Unit.unit = Just Nil
    | otherwise = Nothing

  g Nil = Just Unit.unit
  g _ = Nothing

cons :: forall a. Iso (Tuple a (List a)) (List a)
cons = Iso f g
  where
  f (Tuple x xs) = Just (Cons x xs)

  g (Cons x xs) = Just (Tuple x xs)
  g _ = Nothing

listCases :: forall a. Iso (Either Unit (Tuple a  (List a))) (List a)
listCases = Iso f g
  where
  f (Left x)
    | x == Unit.unit = Just Nil
    | otherwise = Nothing
  f (Right (Tuple x xs)) = Just (Cons x xs)

  g Nil = Just (Left Unit.unit)
  g (Cons x xs) = Just (Right (Tuple x xs))

left :: forall a b. Iso a (Either a b)
left = Iso (Just <<< Left) g
  where
  g = case _ of
    Left x -> Just x
    Right _ -> Nothing

right :: forall a b. Iso b (Either a b)
right = Iso (Just <<< Right) hush

nothing :: forall a. Iso Unit (Maybe a)
nothing = Iso (const $ Just Nothing) g
  where
  g = case _ of
    Nothing -> Just Unit.unit
    _ -> Nothing

just :: forall a. Iso a (Maybe a)
just = Iso (Just <<< Just) id
