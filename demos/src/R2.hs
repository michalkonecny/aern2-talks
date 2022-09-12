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

towardsPt :: (CanBeCReal t) => t -> R2 -> R2 -> R2
towardsPt r (Point2D x1 y1) (Point2D x2 y2) = 
  Point2D (x1*(1-rR) + x2*rR) (y1*(1-rR) + y2*rR)
  where
  rR = creal r

data Polygon t = Polygon [t]
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

ballsToJSON :: [Ball R2] -> String
ballsToJSON pballs =
  printf "balls = [%s]" $ intercalate ",\n" $ map b pballs
  where
    b :: Ball R2 -> String
    b (Ball c r) =
      printf "{ \"c\": %s, \"r\": %s }" (p c) (show $ d r)
    p :: R2 -> String
    p (Point2D x y) =
      printf "{ \"x\": %s, \"y\": %s }" (show $ d x) (show $ d y)
    d :: CReal -> Double
    d = double . centre . unCN . (\r -> r ? (bits 53))

cballsToJSON :: [(Ball R2, Integer)] -> String
cballsToJSON cballs =
  printf "balls = [%s]" $ intercalate ",\n" $ map cb cballs
  where
    cb :: (Ball R2, Integer) -> String
    cb (ball, c) =
      printf "{ \"b\": %s, \"c\": '%s' }" (b ball) colour
      where
        colour = colours !! (c `mod` (length colours))
        colours = ["red", "orange", "green", "blue"]
    b :: Ball R2 -> String
    b (Ball c r) =
      printf "{ \"c\": %s, \"r\": %s }" (p c) (show $ d r)
    p :: R2 -> String
    p (Point2D x y) =
      printf "{ \"x\": %s, \"y\": %s }" (show $ d x) (show $ d y)
    d :: CReal -> Double
    d = double . centre . unCN . (\r -> r ? (bits 53))

cpolysToJSON :: [(Polygon R2, Integer)] -> String
cpolysToJSON cpolys =
  printf "polys = [%s]" $ intercalate ",\n" $ map cp cpolys
  where
    cp :: (Polygon R2, Integer) -> String
    cp (poly, c) =
      printf "{ \"p\": %s, \"c\": '%s' }" (pp poly) colour
      where
        colour = colours !! (c `mod` (length colours))
        colours = ["red", "orange", "green", "blue", "pink", "purple", "cyan", "brown", "gold"]
    pp :: Polygon R2 -> String
    pp (Polygon points) =
      printf "[ %s ]" $ intercalate "," $ map p points
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

pballsToJSON :: [PavingBall R2] -> String
pballsToJSON pballs =
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
