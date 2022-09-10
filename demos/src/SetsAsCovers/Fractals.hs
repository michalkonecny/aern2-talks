{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module SetsAsCovers.Fractals
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
-- Computing a fractal drawing
---------------------------------------

simpleTriangleBalls :: Integer -> [Ball R2]
simpleTriangleBalls levels = 
  iter levels ball0
  where
  ball0 = Ball (pt 0.5 0.5) (creal 0.5)

  iter 0 ball = [ball]
  iter n ball =
    [ballLB] ++ (iter (n-1) ballLT) ++ (iter (n-1) ballRB)
    where
    ((ballLB, ballLT), ballRB) = process ball

    process (Ball (Point2D x y) r) =
      ((ballLB, ballLT), ballRB)
      where
      ballLB = Ball (pt (x-r') (y-r')) r'
      ballLT = Ball (pt (x-r') (y+r')) r'
      ballRB = Ball (pt (x+r') (y-r')) r'
      r' = r / 2

sierpinskiTrianglePoly :: Integer -> [Polygon R2]
sierpinskiTrianglePoly levels = 
    iterateABC levels covering0
    where
    covering0 = [Polygon [a,b,c]]

    a = pt (-1) (-0.9)
    b = pt (1) (-0.9)
    c = pt 0 (-0.9 + (sqrt 3))

    -- iterateABC :: Integer -> [Ball R2] -> [Ball R2]
    iterateABC 0 covering = covering
    iterateABC n covering = iterateABC (n - 1) covering'
      where
      covering' = concat [ [to a t, to b t, to c t]| t <- covering ]

    to p (Polygon points) = 
      Polygon (map (midPt p) points)

sierpinskiTriangle4CBalls :: Integer -> [(Ball R2, Integer)]
sierpinskiTriangle4CBalls levels = 
    iterateABC levels covering0
    where
    covering0 = [(Ball (pt 0 0) (creal 1), 0)]

    a = pt (-1) (-0.9)
    b = pt (1) (-0.9)
    c = pt 0 (-0.9 + (sqrt 3))
    d = pt 0 (-0.9 + (sqrt 3)/3)

    -- iterateABC :: Integer -> [Ball R2] -> [Ball R2]
    iterateABC 0 covering = covering
    iterateABC n covering = iterateABC (n - 1) covering'
      where
      covering' = concat [ zip [to a t, to b t, to c t, to d t] [1..]| (t, _) <- covering ]

    to p (Ball c r) = 
      Ball (midPt p c) (r / 2)

sierpinskiTriangle4CPoly :: Integer -> [(Polygon R2, Integer)]
sierpinskiTriangle4CPoly levels = 
    iterateABC levels covering0
    where
    covering0 = [(Polygon [a,b,c], 1)]

    a = pt (-1) (-0.9)
    b = pt (1) (-0.9)
    c = pt 0 (-0.9 + (sqrt 3))
    d = pt 0 (-0.9 + (sqrt 3)/3)

    -- iterateABC :: Integer -> [Ball R2] -> [Ball R2]
    iterateABC 0 covering = covering
    iterateABC n covering = iterateABC (n - 1) covering'
      where
      covering' = concat [ zip [to a t, to b t, to c t, to d t] [1..]| (t, _) <- covering ]

    to p (Polygon points) = 
      Polygon (map (midPt p) points)

