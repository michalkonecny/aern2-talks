{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module R2
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

data R2 = Point2D { x :: CReal, y :: CReal }
  deriving (Show)

pt :: (CanBeCReal x, CanBeCReal y) => x -> y -> R2
pt x y = Point2D { x = creal x, y = creal y }

midPt :: R2 -> R2 -> R2
midPt (Point2D x1 y1) (Point2D x2 y2) = Point2D ((x1 + x2)/2) ((y1 + y2)/2)

data Triangle t = Triangle t t t
  deriving (Show)

data Ball t = Ball t CReal
  deriving (Show)

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
