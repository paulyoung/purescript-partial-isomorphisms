module Control.Isomorphism.Partial where

import Prelude hiding ((<$>),apply,map)

import Data.Maybe (Maybe(..))


data Iso a b = Iso (a -> Maybe b) (b -> Maybe a)

instance semigroupoidIso :: Semigroupoid Iso where
  compose g f = Iso (apply f >=> apply g) (unapply g >=> unapply f)

instance categoryIso :: Category Iso where
  id = Iso Just Just


inverse :: forall a b. Iso a b -> Iso b a
inverse (Iso f g) = Iso g f

apply :: forall a b. Iso a b -> a -> Maybe b
apply (Iso f _) = f

unapply :: forall a b. Iso a b -> b -> Maybe a
unapply = apply <<< inverse


class IsoFunctor f where
  map :: forall a b. Iso a b -> f a -> f b

infix 5 map as <$>
