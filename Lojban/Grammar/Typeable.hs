{-# OPTIONS -XPolyKinds -XKindSignatures -XDataKinds -XScopedTypeVariables #-}
{-# OPTIONS -XFlexibleInstances -XUndecidableInstances #-}

module Lojban.Grammar.Typeable
(
    Typeable(..), Proxy(..)
)
where

import Data.Typeable (TypeRep, mkAppTy)
import qualified Data.Typeable as T

data Proxy t = Proxy

class Typeable t where
  typeOf :: Proxy t -> TypeRep

instance (Typeable t, Typeable a) => Typeable (t a) where
  typeOf = \_ -> rep
    where
        rep = typeOf (Proxy :: Proxy t) `mkAppTy`
              typeOf (Proxy :: Proxy a)

instance (Typeable t, Typeable a) => T.Typeable (t a) where
  typeOf = \_ -> rep
    where
        rep = typeOf (Proxy :: Proxy t) `mkAppTy`
              typeOf (Proxy :: Proxy a)
