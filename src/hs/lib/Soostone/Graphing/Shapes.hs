------------------------------------------------------------------------------

-- | Holds D3 level primitives.
--   TODO breka me up as necessary!

------------------------------------------------------------------------------

{-# LANGUAGE QuasiQuotes #-}

module Soostone.Graphing.Shapes where

import Text.InterpolatedString.Perl6

import Soostone.Graphing.Base

------------------------------------------------------------------------------

-- | A graph's anchor in it's current context.

data Anchor = Top | RightSide | Bottom | LeftSide

-- | Orientation type represents ... orientation!

data Orientation = Horizontal | Vertical

-- | Wraps a Graph in an SVG group.

group :: Graph () -> Graph ()
group = withTarget (node "g")

-- | Wraps a Graph in an SVG rect.

rect :: Graph () -> Graph ()
rect inner = withTarget (node "rect") $ do
    defaultSize
    inner

defaultSize :: Graph ()
defaultSize = do
    attr "x" (0 :: Int)
    attr "y" (0 :: Int)
    attr "width" (1 :: Int)
    attr "height" (1 :: Int)

-- | Anchors the __target__ 

anchor :: Anchor -> Double -> Graph ()
anchor Bottom height = do
    attr "y" (1 - height)
    anchor Top height

anchor Top height = do
    attr "width" (1 :: Int)
    attr "height" height

anchor RightSide height = do
    attr "x" (1 - height)
    anchor LeftSide height

anchor LeftSide height = do
    attr "width" height
    attr "height" (1 :: Int)

-- | Given a list of `Graph`s, renders them all side-by-side

split :: Orientation -> [Graph ()] -> Graph ()
split ori xs =  
    group . sequence_ . fmap wrap . zip xs $ idxs

    where 
        idxs :: [Int]
        idxs = [0 ..]

        width :: Double
        width = 
            1.0 / fromIntegral (length xs)

        trans :: Int -> String
        trans offset = case ori of
            Vertical   -> [qq| scale($width, 1.0) translate($offset) |]
            Horizontal -> [qq| scale(1.0, $width) translate(0, $offset) |]

        wrap (rect', offset) = group $ do
            rect'' <- extract rect'
            attr "transform"  (trans offset)
            insert rect''

-- | Pads a graph with some space

pad :: Double -> Graph () -> Graph ()
pad sp gr = group $ do
    attr "transform" (scale 0.9) 
    gr

    where
        scale :: Double -> String
        scale x = [qq| scale($sp, $sp) translate({off x}, {off x}) |]
        off x = (1.0 - x) / 2.0

------------------------------------------------------------------------------
