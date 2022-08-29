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
import MixedTypesNumPrelude
import qualified Numeric.CollectErrors as CN
-- import AERN2.MP.WithCurrentPrec

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

openSetBallR2 :: Ball R2 -> OpenSet R2
openSetBallR2 b = OpenSet {isInOpenSet}
  where
    isInOpenSet qb = qb `subBallInnerR2` b

data ClosedSet t
  = ClosedSet {isOutsideClosedSet :: SemiComputablePredBall t}
  | ClosedSetBall {closedSetBall :: Ball t}

closedSetBallR2 :: Ball R2 -> ClosedSet R2
closedSetBallR2 b = ClosedSet {isOutsideClosedSet}
  where
    isOutsideClosedSet qb = ballsDisjoint qb b

newtype CompactSet t = CompactSet {closedSetIsOutside :: SemiComputablePred (ClosedSet t)}

newtype OvertSet t = OvertSet {openSetIntersects :: SemiComputablePred (OpenSet t)}

-- -- a function with "memory", like a
-- type MFn m t1 t2 = t1 -> m -> (t2, m)

-- type SemiComputablePredM m t = MFn m t CSierpinskianWithFalse

-- newtype CompactSetM m t = CompactSetM { closedSetIsOutsideM :: SemiComputablePredM m (ClosedSet t) }
-- newtype OvertSetM m t = OvertSetM { openSetIntersectsM :: SemiComputablePredM m (OpenSet t) }

data Grapic t = Graphic
  { interior :: OpenSet t,
    compact :: CompactSet t,
    overt :: OvertSet t
  }

paveGraphic :: Precision -> Ball R2 -> Integer -> Grapic R2 -> [PavingBall R2]
paveGraphic
  pr
  ball0
  maxDepth
  (Graphic interior (CompactSet {closedSetIsOutside}) (OvertSet {openSetIntersects})) =
    pave 0 ball0
    where
      isInside ball = case interior of
        OpenSet f -> f ball
        OpenSetBall openBall -> (ball `subBallR2` openBall)
      pave depth ball
        | ballInteriorIsInside =
          [PavingBall {ball, isInside = CertainTrue, isOutside = CertainFalse, intersects = CertainTrue}]
        | ballIsOutside =
          [PavingBall {ball, isInside = CertainFalse, isOutside = CertainTrue, intersects = CertainFalse}]
        | depth < maxDepth =
          concat $ map (pave (depth + 1)) $ splitBallOverlapR2 ball
        -- concat $ map (pave (depth + 1)) $ splitBallR2 ball
        | ballIntersects =
          [PavingBall {ball, isInside = TrueOrFalse, isOutside = CertainFalse, intersects = CertainTrue}]
        | otherwise =
          [PavingBall {ball, isInside = TrueOrFalse, isOutside = TrueOrFalse, intersects = TrueOrFalse}]
        where
          ballInteriorIsInside = isTrueCN $ (isInside ball) ? pr
          ballIsOutside = isTrueCN $ closedSetIsOutside (ClosedSetBall ball) ? pr
          ballIntersects = isTrueCN $ openSetIntersects (OpenSetBall ball) ? pr
          -- experimenting with open and closed sets represented fully by characteristic functions: (VERY SLOW, possibly broken)
          -- ballIsOutside = isTrueCN $ closedSetIsOutside (closedSetBallR2 ball) ? pr
          -- ballIntersects = isTrueCN $ openSetIntersects (openSetBallR2 ball) ? pr

isTrueCN :: CN Kleenean -> Bool
isTrueCN ckCN = case CN.toEither ckCN of
  Right v -> isCertainlyTrue v
  Left _ -> False

-- helper functions for defining compact and overt sets:

closedSetIsOutsideFromBallFn ::
  Ball R2 ->
  (Ball R2 -> CSierpinskianWithFalse) ->
  (ClosedSet R2 -> CSierpinskianWithFalse)
closedSetIsOutsideFromBallFn canvas isOutside = closedSetIsOutside
  where
    closedSetIsOutside (ClosedSetBall b) = isOutside b
    closedSetIsOutside (ClosedSet {isOutsideClosedSet}) =
      search 0 canvas
      where
        maxDepth = 7
        search n b
          | n >= maxDepth = ckleenean TrueOrFalse -- error "closedSetIsOutsideFromBallFn: reached maxDepth"
          | otherwise =
            isOutsideClosedSet b
              || foldl1 (&&) (map (search (n + 1)) $ subBalls)
          where
            subBalls = filter (not . isCertain . isOutside) $ splitBallR2 b
            isCertain ck = isCertainlyTrue $ (ck ? prec (10 + n * 4))

openSetIntersectsFromBallFn ::
  Ball R2 ->
  (Ball R2 -> CSierpinskianWithFalse) ->
  (OpenSet R2 -> CSierpinskianWithFalse)
openSetIntersectsFromBallFn canvas intersects = openSetIntersects
  where
    openSetIntersects (OpenSetBall b) = intersects b
    openSetIntersects (OpenSet {isInOpenSet}) =
      search 0 canvas
      where
        maxDepth = 7
        search n b
          | n >= maxDepth = ckleenean TrueOrFalse -- error "openSetIntersectsFromBallFn: reached maxDepth"
          | otherwise =
            (isInOpenSet b && (intersects b))
              || foldl1 (||) (map (search (n + 1)) $ subBalls)
          where
            subBalls = filter (isPossible . intersects) $ splitBallR2 b
            -- isCertain ck = isCertainlyTrue $ (ck ? prec (10 + n * 4))
            isPossible ck = not . isCertainlyFalse $ (ck ? prec (10 + n * 4))
