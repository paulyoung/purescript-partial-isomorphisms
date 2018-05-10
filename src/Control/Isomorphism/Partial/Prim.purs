module Control.Isomorphism.Partial.Prim where

import Prelude hiding (apply)

import Control.Apply (lift2)
import Control.Isomorphism.Partial (Iso(..), apply, unapply)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Unit as Unit

ignore :: forall a. a -> Iso a Unit
ignore x = Iso f g where
  f _ =  Just Unit.unit

  g y
    | y == Unit.unit = Just x
    | otherwise = Nothing

-- | The product type constructor `Tuple` is a bifunctor from `Iso` x `Iso` to
-- | `Iso`, so that we have the bifunctorial map `***` which allows two separate
-- | isomorphisms to work on the two components of a tuple.
split :: forall a b c d. Iso a b -> Iso c d -> Iso (Tuple a c) (Tuple b d)
split i j = Iso f g
  where
  f (Tuple a b) = lift2 Tuple (apply i a) (apply j b)
  g (Tuple c d) = lift2 Tuple (unapply i c) (unapply j d)

infixl 9 split as ***

-- | Nested products associate.
associate :: forall a b c. Iso (Tuple a (Tuple b c)) (Tuple (Tuple a b) c)
associate = Iso f g
  where
  f (Tuple a (Tuple b c)) = Just (Tuple (Tuple a b) c)
  g (Tuple (Tuple a b) c) = Just (Tuple a (Tuple b c))

-- | Products commute.
commute :: forall a b. Iso (Tuple a b) (Tuple b a)
commute = Iso f g
  where
  f (Tuple a b) = Just (Tuple b a)
  g (Tuple b a) = Just (Tuple a b)

-- | `Data.Unit.unit` is the unit element for products.
unit :: forall a. Iso a (Tuple a Unit)
unit = Iso f g
  where
  f a = Just (Tuple a Unit.unit)

  g (Tuple a b)
    | b == Unit.unit = Just a
    | otherwise = Nothing

-- | Products distribute over sums.
distribute
  :: forall a b c
   . Iso (Tuple a (Either b c)) (Either (Tuple a b) (Tuple a c))
distribute = Iso f g
  where
  f (Tuple a (Left b)) = Just (Left (Tuple a b))
  f (Tuple a (Right c)) = Just (Right (Tuple a c))

  g (Left (Tuple a b)) = Just (Tuple a (Left b))
  g (Right (Tuple a b)) = Just (Tuple a (Right b))

-- | `element x` is the partial isomorphism between `Data.Unit.unit` and the
-- | singleton set which contains just `x`.
element :: forall a. Eq a => a -> Iso Unit a
element x = Iso f g
  where
  f _ = Just x

  g b | b == x = Just Unit.unit
  g _ = Nothing

-- | For a predicate `p`, `subset p` is the identity isomorphism restricted to
-- | elements matching the predicate.
subset :: forall a. (a -> Boolean) -> Iso a a
subset p = Iso f f
  where
  f x | p x = Just x
  f _ = Nothing

iterate :: forall a. Iso a a -> Iso a a
iterate step = Iso f g
  where
  f = Just <<< driver (apply step)
  g = Just <<< driver (unapply step)

  driver :: (a -> Maybe a) -> (a -> a)
  driver step' state = case step' state of
    Just state' -> driver step' state'
    Nothing -> state
