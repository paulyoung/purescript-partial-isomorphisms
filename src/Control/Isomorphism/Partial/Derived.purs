module Control.Isomorphism.Partial.Derived where

import Prelude hiding (unit)

import Control.Isomorphism.Partial (Iso, inverse)
import Control.Isomorphism.Partial.Constructors (cons, nil)
import Control.Isomorphism.Partial.Prim (associate, iterate, unit, (***))
import Data.List (List)
import Data.Tuple (Tuple)

foldl :: forall a b. Iso (Tuple a b) a -> Iso (Tuple a (List b)) a
foldl i =
  inverse unit
    <<< (id *** inverse nil)
    <<< iterate (step i)
  where
  step i' =
    (i' *** id)
      <<< associate
      <<< (id *** inverse cons)
