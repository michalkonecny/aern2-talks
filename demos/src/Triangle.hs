{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Triangle where

import AERN2.MP
import AERN2.Real
import MixedTypesNumPrelude
import qualified Numeric.CollectErrors as CN
-- import AERN2.MP.WithCurrentPrec

import R2
import Sets

---------------------------------------
-- Example sets
---------------------------------------

canvas :: Ball R2
canvas = Ball (pt 0 0) (creal 1)

simpleTriangleCompact :: CompactSet R2
simpleTriangleCompact =
  CompactSet {closedSetIsOutside = closedSetIsOutsideFromBallFn canvas isOutside}
  where
    isOutside (Ball (Point2D x y) r) =
      x - r + y - r > 0

simpleTriangleOvert :: OvertSet R2
simpleTriangleOvert =
  OvertSet {openSetIntersects = openSetIntersectsFromBallFn canvas interiorIntersects}
  where
    interiorIntersects (Ball (Point2D x y) r) =
      x - r + y - r < 0

simpleTriangleGraphic :: Grapic R2
simpleTriangleGraphic =
  Graphic {interior, compact = simpleTriangleCompact, overt = simpleTriangleOvert}
  where
    interior = OpenSet {isInOpenSet}
    isInOpenSet (Ball (Point2D x y) r) =
      x + r + y + r < 0

sierpinskiTriangleCompact :: CompactSet R2
sierpinskiTriangleCompact =
  CompactSet {closedSetIsOutside = closedSetIsOutsideFromBallFn canvas isOutside}
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
            (x - r + y - r > 0) -- above the main diagonal
              || ( (x + r < 0) && (0 < y - r) -- inside top left quadrant
                     && (search (n - 1) (Ball (awayFrom v1 p) (2 * r)))
                 )
              || ( (x + r < 0) && (y + r < 0) -- inside bottom left quadrant
                     && (search (n - 1) (Ball (awayFrom v2 p) (2 * r)))
                 )
              || ( (0 < x - r) && (y + r < 0) -- inside bottom right quadrant
                     && (search (n - 1) (Ball (awayFrom v3 p) (2 * r)))
                 )

sierpinskiTriangleOvert :: OvertSet R2
sierpinskiTriangleOvert =
  OvertSet {openSetIntersects = openSetIntersectsFromBallFn canvas interiorIntersects}
  where
    v1 = pt (-1) (1) -- top left corner
    v2 = pt (-1) (-1) -- bottom left corner
    v3 = pt 1 (-1) -- bottom right corner
    awayFrom (Point2D vx vy) (Point2D px py) = Point2D (2 * px - vx) (2 * py - vy)
    interiorIntersects b =
      search maxDepth b
      where
        maxDepth = 20
        search n (Ball p@(Point2D x y) r)
          | n == 0 = error "sierpinskiTriangleOvert: maxDepth reached"
          | otherwise =
            (not $ x - r + y - r >= 0) -- not outside
              && ( x + r + y + r > 0 -- intersects the main diagonal
                     || (x - r < 0 && x + r > 0) -- intersects the y axis
                     || (y - r < 0 && y + r > 0) -- intersects the x axis
                     || ( (0 <= y - r) && (x + r <= 0) -- inside top left quadrant
                            && (search (n - 1) (Ball (awayFrom v1 p) (2 * r))) -- intersects when zoomed away from v1
                        )
                     || ( (x + r <= 0) && (y + r <= 0) -- inside bottom left quadrant
                            && (search (n - 1) (Ball (awayFrom v2 p) (2 * r))) -- intersects when zoomed away from v2
                        )
                     || ( (0 <= x - r) && (y + r <= 0) -- inside bottom right quadrant
                            && (search (n - 1) (Ball (awayFrom v3 p) (2 * r))) -- intersects when zoomed away from v3
                        )
                 )

sierpinskiTriangleGraphic :: Grapic R2
sierpinskiTriangleGraphic =
  Graphic {interior, compact = sierpinskiTriangleCompact, overt = sierpinskiTriangleOvert}
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
