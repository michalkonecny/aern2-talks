{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module R2 where

import AERN2.MP
import AERN2.Real
import qualified AERN2.Real.Type as CRealType
import Data.List (intercalate)
import qualified Data.List as List
import Data.Maybe (fromJust)
import Math.NumberTheory.Logarithms (integerLog2)
import MixedTypesNumPrelude
import qualified Numeric.CollectErrors as CN
import Text.Printf (printf)

data R2 = Point2D {x :: CReal, y :: CReal}
  deriving (Show)

pt :: (CanBeCReal x, CanBeCReal y) => x -> y -> R2
pt x y = Point2D {x = creal x, y = creal y}

midPt :: R2 -> R2 -> R2
midPt (Point2D x1 y1) (Point2D x2 y2) = Point2D ((x1 + x2) / 2) ((y1 + y2) / 2)

data Triangle t = Triangle t t t
  deriving (Show)

data Ball t = Ball t CReal
  deriving (Show)

splitBallR2 :: Ball R2 -> [Ball R2]
splitBallR2 (Ball (Point2D cx cy) r) =
  [ Ball (Point2D (cx - r') (cy - r')) r',
    Ball (Point2D (cx + r') (cy - r')) r',
    Ball (Point2D (cx - r') (cy + r')) r',
    Ball (Point2D (cx + r') (cy + r')) r'
  ]
  where
    r' = r / 2

splitBallOverlapR2 :: Ball R2 -> [Ball R2]
splitBallOverlapR2 (Ball (Point2D cx cy) r) =
  [ Ball (Point2D (cx - s) (cy - s)) r',
    Ball (Point2D (cx + s) (cy - s)) r',
    Ball (Point2D (cx - s) (cy + s)) r',
    Ball (Point2D (cx + s) (cy + s)) r'
  ]
  where
    r' = 0.51 * r
    s = r - r'

subBallR2 :: Ball R2 -> Ball R2 -> CKleenean
subBallR2 (Ball (Point2D x1 y1) r1) (Ball (Point2D x2 y2) r2) =
  abs (x2 - x1) <= r2 - r1
    && abs (y2 - y1) <= r2 - r1

subBallInnerR2 :: Ball R2 -> Ball R2 -> CKleenean
subBallInnerR2 (Ball (Point2D x1 y1) r1) (Ball (Point2D x2 y2) r2) =
  abs (x2 - x1) < r2 - r1
    && abs (y2 - y1) < r2 - r1

ballsDisjoint :: Ball R2 -> Ball R2 -> CKleenean
ballsDisjoint (Ball (Point2D x1 y1) r1) (Ball (Point2D x2 y2) r2) =
  abs (x2 - x1) > r2 + r1
    || abs (y2 - y1) > r2 + r1

ballR2ToBlurPoint :: Ball R2 -> R2
ballR2ToBlurPoint (Ball (Point2D x y) r) = Point2D xBlur yBlur
  where
    xBlur = CRealType.lift2 (CN.lift2 blur) r x
    yBlur = CRealType.lift2 (CN.lift2 blur) r y
    blur :: MPBall -> MPBall -> MPBall
    blur rad = updateRadius (+ (errorBound rad))

trianglesToJSON :: [Triangle R2] -> String
trianglesToJSON triangles =
  printf "triangles = [%s]" $ intercalate ",\n" $ map tr triangles
  where
    tr :: Triangle R2 -> String
    tr (Triangle a b c) =
      printf "{ \"v1\": %s, \"v2\": %s, \"v3\": %s }" (p a) (p b) (p c)
    p :: R2 -> String
    p (Point2D x y) =
      printf "{ \"x\": %s, \"y\": %s }" (show $ d x) (show $ d y)
    d :: CReal -> Double
    d = double . centre . unCN . (\r -> r ? (bits 53))

data PavingBall t = PavingBall
  { ball :: Ball t,
    isInside :: Kleenean,
    isOutside :: Kleenean,
    intersects :: Kleenean
  }

ballsToJSON :: [PavingBall R2] -> String
ballsToJSON pballs =
  printf "balls = [%s]" $ intercalate ",\n" $ map pb pballs
  where
    pb :: PavingBall R2 -> String
    pb (PavingBall {ball, isInside, isOutside, intersects}) =
      printf "{ \"b\": %s, \"s\": '%s' }" (b ball) status
      where
        status
          | isCertainlyTrue isInside = "inside"
          | isCertainlyTrue isOutside = "outside"
          | isCertainlyTrue intersects = "intersects"
          | otherwise = "unknown"
    b :: Ball R2 -> String
    b (Ball c r) =
      printf "{ \"c\": %s, \"r\": %s }" (p c) (show $ d r)
    p :: R2 -> String
    p (Point2D x y) =
      printf "{ \"x\": %s, \"y\": %s }" (show $ d x) (show $ d y)
    d :: CReal -> Double
    d = double . centre . unCN . (\r -> r ? (bits 53))
