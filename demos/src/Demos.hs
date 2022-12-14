{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Demos 
where

import MixedTypesNumPrelude
import qualified Numeric.CollectErrors as CN

import qualified Data.List as List
import Data.Maybe (fromJust)

import AERN2.MP
import AERN2.Real
-- import AERN2.MP.WithCurrentPrec
import Math.NumberTheory.Logarithms (integerLog2)
import Text.Printf (printf)
import Data.List (intercalate)

import R2

---------------------------------------
-- Haskell basics with MixedTypesNumPrelude
---------------------------------------

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

sine100prec = sin(10000000000) ? (prec 100)
-- [-0.48750602508751069152779429... ± ~2.4498e-30 ~2^(-98)]

sine100bits = sin(10000000000) ? (bits 100)
-- [-0.48750602508751069152779429... ± ~8.3002e-51 ~2^(-166)]

---------------------------------------
-- Logistic iteration
--   need for memoizing intermediate values
---------------------------------------

logistic :: (RealNumber t) => Rational -> Integer -> t -> t
logistic c n x0 =
  (iterate lg x0) !! n
  where
  lg x = c * x * (1-x)

logistic1 :: Integer -> CReal
logistic1 n = logistic 3.82 n (creal 0.5)

{-
ghci> logistic1 15 ? (bits 100)
[0.75380485336892032057596409... ± ~1.3094e-48 ~2^(-159)]
(0.00 secs, 1,367,352 bytes)
-}

-- The following version artificially disables memoization of the parameter x
-- by turning x into a function of a dummy parameter ():

logistic_BAD :: (RealNumber t) => Rational -> Integer -> t -> t
logistic_BAD c n x0 =
  ((iterate lg (\() -> x0)) !! n) ()
  where
  lg x = \() -> c * (x ()) * (1-(x ()))

logistic1_BAD :: Integer -> CReal
logistic1_BAD n = logistic_BAD 3.82 n (creal 0.5)

{-
ghci> logistic1_BAD 15 ? (bits 100)
[0.75380485336892032057596409... ± ~1.3094e-48 ~2^(-159)]
(0.95 secs, 1,448,809,840 bytes)
-}

---------------------------------------
-- Maximum of two reals
--   non-deterministic choice
--   limit
---------------------------------------

real_max :: (RealNumber t) => t -> t -> t
real_max x y = 
  limit $ \(n :: Integer) -> 
    let e = 0.5^n in
      if select (x > y - e) (y > x - e)
        then x
        else y

e = exp 1

real_max_test1 :: CReal
real_max_test1 = real_max (pi - pi) (e - e)

real_max_PAR :: 
  (RealNumber t, 
   HasIfThenElseSameType (OrderCompareType t t) t) 
  =>
  t -> t -> t
real_max_PAR x y =
  if x < y then y else x

real_max_PAR_test1 :: CReal
real_max_PAR_test1 = real_max_PAR (pi - pi) (e - e)

---------------------------------------
-- Magnitude (integer logarithm)
--   partial function
--   non-deterministic integer result
---------------------------------------

magnitude_belowHalf_pre :: (RealNumber t) => t -> CN Integer
magnitude_belowHalf_pre x =
  searchFrom 0
  where
  searchFrom n =
    if select (0.5^^(n+2) < x) (x < 0.5^^(n+1))
      then (cn (-n))
      else searchFrom (n + 1)

magnitude_belowHalf :: (RealNumber t) => t -> CN Integer
magnitude_belowHalf x =
  searchFrom 0
  where
  searchFrom n =
    if select (x <= 0) (-(0.5^^(n+2)) < x)
      then CN.noValueNumErrorCertain $ CN.NumError
            "magnitude_belowHalf called with a non-positive number"
      else
      if select (0.5^^(n+2) < x) (x < 0.5^^(n+1))
        then (cn (-n))
        else searchFrom (n + 1)

magnitude_belowTwo :: (RealNumber t) => t -> CN Integer
magnitude_belowTwo x = (magnitude_belowHalf (x/4)) + 2

magnitude :: (RealNumber t) => t -> CN Integer
magnitude x =
  if select (x < 2) (x > 0.25)
    then magnitude_belowTwo x
    else 2 - (magnitude_belowTwo (1/x))

---------------------------------------
-- Square root via Heron method
---------------------------------------

-- with a specific real number type:

sqrt_approx_specific :: CReal -> Integer -> CReal
sqrt_approx_specific x n =
  let step y = (y + x/y)/2 in
  (iterate step (creal 1)) !! n

-- now using abstract real number type:

sqrt_approx :: (RealNumber t) => t -> Integer -> t
sqrt_approx x n =
  let step y = (y + x/y)/2 in
  (iterate step (r 1)) !! n

r :: (RealNumber r, ConvertibleExactly a r) => a -> r
r = convertExactly

-- need to change rate of convergence to match limit's assumption:

sqrt_approx_fast :: (RealNumber t) => t -> Integer -> t
sqrt_approx_fast x n =
  sqrt_approx x (1 + (integerLog2 (n+1)))
    
-- sqrt restricted to an interval near 1:

restr_sqrt :: (RealNumber t) => t -> t
restr_sqrt x = limit $ sqrt_approx_fast x
    
-- sqrt for positive real numbers:

scale :: (RealNumber t) => t -> (Integer, t)
scale x = (z,y)
  where
  z = (unCN $ magnitude x) `divI` 2
  y = x * 2^^(-2*z)

sqrt_pos :: (RealNumber t) => t -> t
sqrt_pos x = (restr_sqrt y) * 2^^z
  where
  (z,y) = scale x

-- sqrt for non-negative numbers, 0 covered using a limit:

split :: (RealNumber t) => t -> t -> t -> CN Bool
split x y eps = 
  select (y-eps < x) (x - eps < y)

sqrt2 :: (RealNumber t) => t -> t
sqrt2 (x :: t) = limit $ \n ->
  let eps = (r 0.5 :: t)^(n :: Integer) in
  if (split x eps eps)
    then sqrt_pos x 
    else r 0.0

---------------------------------------
-- Finding a root using the intermediate value theorem
---------------------------------------
  
cIVT :: (RealNumber t) => (t -> t) -> t
cIVT (f :: t -> t) = 
  limit $ \(n :: Integer) -> fst $ (iterate aux (r 0, r 1)) !! (2*n)
  where
  aux (a, b) =
    let m1 = (2*a + b)/3 in
    let m2 = (a + 2*b)/3 in
    if select ((f m2) > 0) ((f m1) < 0)
      then (a, m2)
      else (m1, b)
