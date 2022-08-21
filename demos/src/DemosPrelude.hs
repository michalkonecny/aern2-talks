{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module DemosPrelude where
  
import Prelude hiding (pred, succ, (<),(<=),(>),(>=),abs,max,min,not,(&&),(||),pi)
import Numeric.OrdGenericBool ( (<),(<=),(>),(>=) )
import MixedTypesNumPrelude (ifThenElse, CN, unCN, integer)
import qualified Data.List as List
import Data.Maybe (fromJust)

import System.Environment

-- import Criterion.Main

import AERN2.MP
import AERN2.Real
-- import AERN2.MP.WithCurrentPrec
import Math.NumberTheory.Logarithms (integerLog2)

---------------------------------------
-- Haskell basics with standard Prelude
---------------------------------------


one :: Num t => t
one = 1

two = one + one

increment x = x + one

fact1 n = product [one..n]

fact2 n = 
  if n == 0 then n * (fact2 (n - 1)) else one

fact3 0 = one
fact3 n = n * (fact2 (n - 1))

fact4 n = aux one n
  where
  aux prev 0 = prev
  aux prev n = aux (prev * n) (n - 1) -- tail recursion

---------------------------------------
-- AERN2 basics
--   an approximation of a real number
---------------------------------------

sine100prec = sin(10000000000 :: CReal) ? (prec (100 :: Integer))
-- [-0.48750602508751069152779429... ± ~2.4498e-30 ~2^(-98)]

sine100bits = sin(10000000000 :: CReal) ? (bits (100 :: Integer))
-- [-0.48750602508751069152779429... ± ~8.3002e-51 ~2^(-166)]

---------------------------------------
-- Maximum of two reals
--   non-deterministic choice
--   limit
---------------------------------------

real_max :: (Fractional t, RealNumber t) => t -> t -> t
real_max x y = 
  limit $ \(n :: Integer) -> 
    let e = 0.5^n in
    if select (x > y - e) (y > x - e)
      then x
      else y

