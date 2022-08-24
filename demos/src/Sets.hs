{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Sets
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
import qualified AERN2.Real.Type as CRealType

---------------------------------------
-- Types for general subsets
---------------------------------------

data Ball t = Ball { ballCentre :: t, ballRadius :: CReal }
  deriving (Show)

type CSierpinskianWithFalse = CKleenean 
  -- if True, it must show in finite time, ie a generalisation of the Sierpinski space
type SemiComputablePred t = t -> CSierpinskianWithFalse

data OpenSet t 
  = OpenSet { isInOpenSet :: SemiComputablePred t }
  | OpenSetBall { openSetBall :: Ball t }

data ClosedSet t 
  = ClosedSet { isOutsideClosedSet :: SemiComputablePred t }
  | ClosedSetBall { closedSetBall :: Ball t }

newtype CompactSet t = CompactSet { closedSetIsOutside :: SemiComputablePred (ClosedSet t) }
newtype OvertSet t = OvertSet { openSetIntersects :: SemiComputablePred (OpenSet t) }

data Grapic t = Graphic { compact :: CompactSet t, interior :: OpenSet t }


-- a function with "memory", like a 
type MFn m t1 t2 = (t1, m) -> (t2, m)

type SemiComputablePredM m t = MFn m t CSierpinskianWithFalse

newtype CompactSetM m t = CompactSetM { closedSetIsOutsideM :: SemiComputablePredM m (ClosedSet t) }
newtype OvertSetM m t = OvertSetM { openSetIntersectsM :: SemiComputablePredM m (OpenSet t) }

---------------------------------------
-- Types and utilities for R2 subsets
---------------------------------------

data R2 = Point2D { x :: CReal, y :: CReal }
  deriving (Show)

pt :: (CanBeCReal x, CanBeCReal y) => x -> y -> R2
pt x y = Point2D { x = creal x, y = creal y }

midPt :: R2 -> R2 -> R2
midPt (Point2D x1 y1) (Point2D x2 y2) = Point2D ((x1 + x2)/2) ((y1 + y2)/2)

splitBallR2 :: Ball R2 -> [Ball R2]
splitBallR2 (Ball (Point2D cx cy) r) =
  [Ball (Point2D (cx-r') (cy-r')) r'
  ,Ball (Point2D (cx+r') (cy-r')) r'
  ,Ball (Point2D (cx-r') (cy+r')) r'
  ,Ball (Point2D (cx+r') (cy+r')) r'
  ]
  where
  r' = r/2

subBallR2 :: Ball R2 -> Ball R2 -> CKleenean
subBallR2 (Ball (Point2D x1 y1) r1) (Ball (Point2D x2 y2) r2) =
  abs(x2-x1) <= r2-r1
  && 
  abs(y2-y1) <= r2-r1

ballR2ToBlurPoint :: Ball R2 -> R2
ballR2ToBlurPoint (Ball (Point2D x y) r) = Point2D xBlur yBlur
  where
  xBlur = CRealType.lift2 (CN.lift2 blur) r x
  yBlur = CRealType.lift2 (CN.lift2 blur) r y
  blur :: MPBall -> MPBall -> MPBall
  blur rad = updateRadius (+ (errorBound rad))

paveCompact :: Precision -> Ball R2 -> Integer -> CompactSet R2 -> [Ball R2]
paveCompact pr ball0 maxDepth (CompactSet { closedSetIsOutside }) = 
  pave 0 ball0
  where
  pave depth ball =
    let isOutsideCN = closedSetIsOutside (ClosedSetBall ball) ? pr in
    case CN.toEither isOutsideCN of
      Right CertainTrue -> []
      _ | depth >= maxDepth -> [ball]
      _ -> concat $ map (pave (depth + 1)) $ splitBallR2 ball

paveGraphic :: Precision -> Ball R2 -> Integer -> Grapic R2 -> [Ball R2]
paveGraphic pr ball0 maxDepth (Graphic (CompactSet { closedSetIsOutside }) interior) = 
  pave 0 ball0
  where
  isInside ball = case interior of
    OpenSet f -> f $ ballR2ToBlurPoint ball
    OpenSetBall openBall -> ball `subBallR2` openBall
  pave depth ball =
    let isOutsideCN = closedSetIsOutside (ClosedSetBall ball) ? pr in
    let interiorIsInsideCN = isInside ball ? pr in
    case (CN.toEither isOutsideCN, CN.toEither interiorIsInsideCN) of
      (Right CertainTrue, _) -> []
      (_, Right CertainTrue) -> [ball]
      _ | depth >= maxDepth -> [ball]
      _ -> concat $ map (pave (depth + 1)) $ splitBallR2 ball

unitBall :: Ball R2
unitBall = Ball (pt 0 0) (creal 1)

ballsToJSON :: [Ball R2] -> String
ballsToJSON balls =
  printf "balls = [%s]" $ intercalate ",\n" $ map tr balls
  where
  tr :: Ball R2 -> String
  tr (Ball c r) =
    printf "{ \"c\": %s, \"r\": %s }" (p c) (show $ d r)
  p :: R2 -> String
  p (Point2D x y) =
    printf "{ \"x\": %s, \"y\": %s }" (show $ d x) (show $ d y)
  d :: CReal -> Double
  d = double . centre . unCN . (\r -> r ? (bits 53))

---------------------------------------
-- Example sets
---------------------------------------

simpleTriangleCompact :: CompactSet R2
simpleTriangleCompact = CompactSet { closedSetIsOutside }
  where
  closedSetIsOutside (ClosedSetBall (Ball (Point2D x y) r)) =
    x + y > 2*r
  closedSetIsOutside _ = undefined

simpleTriangleGraphic :: Grapic R2
simpleTriangleGraphic = Graphic {compact=simpleTriangleCompact, interior}
  where
  interior = OpenSet {isInOpenSet}
  isInOpenSet (Point2D x y) = 
    x + y <= 0

-- sierpinskiTriangleCompact :: CompactSetM R2
-- sierpinskiTriangleCompact = CompactSetM { isEmptyClosedSetM }
--   where



