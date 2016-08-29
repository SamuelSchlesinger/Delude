{-|
Module      : Delude
Description : An alternative Prelude
Copyright   : (c) Samuel Schlesinger, 2016
License     : MIT
Maintainer  : SGSchlesinger@gmail.com
Stability   : experimental
Portability : portable

This module is the result of my dislike for the standard
Haskell Prelude, and the current replacements (excepting Subhask)
not having good enough solutions in my opinion for certain
problems such as the numerical class hierarchy.
-}

{-# LANGUAGE 
    NoImplicitPrelude
  , TypeOperators
  , FlexibleContexts
  , FlexibleInstances
  , ConstraintKinds
  , KindSignatures
  , TypeInType
  , RebindableSyntax
  , EmptyCase 
  , TypeFamilies
  , DefaultSignatures
  , ConstrainedClassMethods
  , ScopedTypeVariables
  , MultiParamTypeClasses
  , RankNTypes
  , GADTs #-}

module Delude 
  (
    module Data.Kind
  , module Data.Bool
  , Arrow(..)
  , Category(..)
  ) where

import Data.Kind
import Data.Bool
import GHC.Exts

-- | Associative operator f >=> (g >=> h) ~ (f >=> g) >=> h
class Arrow (arrow :: Type -> Type -> Type) where
  (>=>) :: arrow a b -> arrow b c -> arrow a c
  a >=> b = b <=< a 
  (<=<) :: arrow b c -> arrow a b -> arrow a c
  a <=< b = b >=> a

-- | Concrete arrows have a faithful functor to Hask
class Arrow arr => Concrete (arr :: Type -> Type -> Type) where
  ($) :: arr a b -> a -> b

-- | Co-Concrete arrows have a faithful functor from Hask
class Arrow arr => CoConcrete (arr :: Type -> Type -> Type) where
  replace :: (a -> b) -> arr a b

-- | An arrow with an identity operation is a category
class Arrow arrow => Category (arrow :: Type -> Type -> Type) where
  id :: arrow a a

instance Arrow (->) where
  (f >=> g) x = g (f x)
  (f <=< g) x = f (g x)

instance Category (->) where
  id x = x

instance Concrete (->) where
  f $ x = f x

instance CoConcrete (->) where
  replace f = f

class (Arrow c, Arrow d) => Functor c d f where
  fmap :: c x y -> d (f x) (f y)

class (Arrow c, Arrow d) => c <: d where
  reduce :: c x y -> d x y

instance Arrow a => a <: a where
  reduce = id

-- I really wish that I could make subtyping transitive but that just does not work

data Constrained a c where
  Constrained :: { unConstrain :: c => a  } -> Constrained a c

