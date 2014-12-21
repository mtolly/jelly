module Jelly.Prelude
( module Prelude
, module Control.Monad
, module Control.Applicative
, (>>), (<<), (—)
) where

import Prelude hiding ((>>), return, fail)
import Control.Monad hiding (liftM, liftM2, liftM3, liftM4, liftM5, ap, (>>), return, fail)
import Control.Applicative hiding ((<*), (*>))
import qualified Control.Applicative as A

(>>) :: (A.Applicative f) => f () -> f a -> f a
(>>) = (A.*>)
infixl 1 >>

(<<) :: (A.Applicative f) => f a -> f () -> f a
(<<) = (A.<*)
infixl 1 <<

-- | Turn function call arguments into bulleted lists.
(—) :: (a -> b) -> a -> b
(—) = ($)
infixl 0 —
