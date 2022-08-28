{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Sets where

import AERN2.MP
import AERN2.Real
-- import AERN2.MP.WithCurrentPrec

import Data.List (intercalate)
import qualified Data.List as List
import Data.Maybe (fromJust)
import Math.NumberTheory.Logarithms (integerLog2)
import MixedTypesNumPrelude
import qualified Numeric.CollectErrors as CN
import R2
import Text.Printf (printf)

---------------------------------------
-- Types for general subsets
---------------------------------------

type SierpinskianWithFalse = Kleenean

type CSierpinskianWithFalse = CKleenean

-- if True, it must show in finite time, ie a generalisation of the Sierpinski space
type SemiComputablePred t = t -> CSierpinskianWithFalse

type SemiComputablePredBall t = Ball t -> CSierpinskianWithFalse

data OpenSet t
  = OpenSet {isInOpenSet :: SemiComputablePredBall t}
  | OpenSetBall {openSetBall :: Ball t}

data ClosedSet t
  = ClosedSet {isOutsideClosedSet :: SemiComputablePredBall t}
  | ClosedSetBall {closedSetBall :: Ball t}

newtype CompactSet t = CompactSet {closedSetIsOutside :: SemiComputablePred (ClosedSet t)}

newtype OvertSet t = OvertSet {openSetIntersects :: SemiComputablePred (OpenSet t)}

data Grapic t = Graphic
  { compact :: CompactSet t,
    -- overt :: OvertSet t,
    interior :: OpenSet t
  }

-- data Grapic t = Graphic { compact :: CompactSet t, overt :: OvertSet t, interior :: OpenSet t } -- TODO

closedSetIsOutsideFromBallFn ::
  (Ball R2 -> CSierpinskianWithFalse) ->
  (ClosedSet R2 -> CSierpinskianWithFalse)
closedSetIsOutsideFromBallFn isOutside = closedSetIsOutside
  where
    closedSetIsOutside (ClosedSetBall b) = isOutside b
    closedSetIsOutside (ClosedSet {isOutsideClosedSet}) =
      -- error "simpleTriangleCompact: isOutsideClosedSet not implemented yet"
      search 0 unitBall
      where
        search n b =
          isOutsideClosedSet b
            || foldl1 (&&) (map (search (n + 1)) $ subBalls)
          where
            subBalls = filter (not . isCertain . isOutside) $ splitBallR2 b
            isCertain ck = isCertainlyTrue $ (ck ? prec (10 + n * 4))

-- -- a function with "memory", like a
-- type MFn m t1 t2 = t1 -> m -> (t2, m)

-- type SemiComputablePredM m t = MFn m t CSierpinskianWithFalse

-- newtype CompactSetM m t = CompactSetM { closedSetIsOutsideM :: SemiComputablePredM m (ClosedSet t) }
-- newtype OvertSetM m t = OvertSetM { openSetIntersectsM :: SemiComputablePredM m (OpenSet t) }

-- paveCompact :: Precision -> Ball R2 -> Integer -> CompactSet R2 -> [Ball R2]
-- paveCompact pr ball0 maxDepth (CompactSet { closedSetIsOutside }) =
--   pave 0 ball0
--   where
--   pave depth ball =
--     let isOutsideCN = closedSetIsOutside (ClosedSetBall ball) ? pr in
--     case CN.toEither isOutsideCN of
--       Right CertainTrue -> []
--       _ | depth >= maxDepth -> [ball]
--       _ -> concat $ map (pave (depth + 1)) $ splitBallR2 ball

paveGraphic :: Precision -> Ball R2 -> Integer -> Grapic R2 -> [Ball R2]
paveGraphic pr ball0 maxDepth (Graphic (CompactSet {closedSetIsOutside}) interior) =
  pave 0 ball0
  where
    isInside ball = case interior of
      OpenSet f -> f ball
      OpenSetBall openBall -> (ball `subBallR2` openBall)
    pave depth ball =
      let isOutsideCN = closedSetIsOutside (ClosedSetBall ball) ? pr
       in let interiorIsInsideCN = (isInside ball) ? pr
           in case (CN.toEither isOutsideCN, CN.toEither interiorIsInsideCN) of
                (Right CertainTrue, _) -> []
                (_, Right CertainTrue) -> [ball]
                _ | depth >= maxDepth -> [ball]
                _ -> concat $ map (pave (depth + 1)) $ splitBallR2 ball

unitBall :: Ball R2
unitBall = Ball (pt 0 0) (creal 1)

---------------------------------------
-- Example sets
---------------------------------------

simpleTriangleCompact :: CompactSet R2
simpleTriangleCompact =
  CompactSet {closedSetIsOutside = closedSetIsOutsideFromBallFn isOutside}
  where
    isOutside (Ball (Point2D x y) r) =
      x - r + y - r > 0

simpleTriangleGraphic :: Grapic R2
simpleTriangleGraphic = Graphic {compact = simpleTriangleCompact, interior}
  where
    interior = OpenSet {isInOpenSet}
    isInOpenSet (Ball (Point2D x y) r) =
      x + r + y + r < 0 && x - r > 0 && y - r > 0

sierpinskiTriangleCompact :: CompactSet R2
sierpinskiTriangleCompact =
  CompactSet {closedSetIsOutside = closedSetIsOutsideFromBallFn isOutside}
  where
    v1 = pt (-1) (1)
    v2 = pt (-1) (-1)
    v3 = pt 1 (-1)
    awayFrom (Point2D vx vy) (Point2D px py) = Point2D (2 * px - vx) (2 * py - vy)
    isOutside b =
      search maxDepth b
      where
        maxDepth = 20
        search n (Ball p@(Point2D x y) r)
          | n == 0 = error "sierpinskiTriangleCompact: maxDepth reached"
          | otherwise =
            (x + y > 2 * r)
              || ((x < 0) && (y + r > 0) && (search (n - 1) (Ball (awayFrom v1 p) (2 * r))))
              || ((x + r < 0) && (y + r < 0) && (search (n - 1) (Ball (awayFrom v2 p) (2 * r))))
              || ((x + r > 0) && (y < 0) && (search (n - 1) (Ball (awayFrom v3 p) (2 * r))))

sierpinskiTriangleGraphic :: Grapic R2
sierpinskiTriangleGraphic =
  Graphic {compact = sierpinskiTriangleCompact, interior}
  where
    interior = OpenSet {isInOpenSet}
    isInOpenSet _ = ckleenean False -- this shape has no interior

-- data NestedCover =
--   NestedCover { oneBallCover :: Ball R2, maybeSubCover :: Maybe [NestedCover] }

-- sierpinskiTriangleCompact :: CompactSetM (Maybe NestedCover) R2
-- sierpinskiTriangleCompact = CompactSetM { closedSetIsOutsideM }
--   where
--   closedSetIsOutsideM (ClosedSetBall (Ball (Point2D x y) r)) maybePrevCover =
--     undefined
--     where
--     prevCover = maybe (NestedCover unitBall Nothing) id maybePrevCover
--     searchCover (NestedCover {oneBallCover, maybeSubCover}) =
--       undefined

--   closedSetIsOutsideM _ prevCover =
--     (ckleenean TrueOrFalse, prevCover)
--     -- should implement this properly, but it is not needed for paving
