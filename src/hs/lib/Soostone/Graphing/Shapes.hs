------------------------------------------------------------------------------

-- | Holds D3 level primitives.
--   TODO break me up as necessary!

------------------------------------------------------------------------------

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExtendedDefaultRules  #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Soostone.Graphing.Shapes where

import Language.Javascript.JMacro

import Soostone.Graphing.Base

------------------------------------------------------------------------------

-- | A graph's anchor in it's current context.

data Anchor = Top | RightSide | Bottom | LeftSide

-- | Orientation type represents ... orientation!

data Orientation = Horizontal | Vertical

-- | Wraps a Graph in an SVG group.

group :: Graph a () -> Graph a ()
group = append "g"

-- | Wraps a Graph in an SVG rect.

rect :: Graph a () -> Graph a ()
rect inner = append "rect" $ do
    defaultSize
    inner

defaultSize :: Graph a ()
defaultSize = do
    attr "x" 0
    attr "y" 0
    attr "width" 1
    attr "height" 1

-- | Anchors the __target__ 

anchor :: Anchor -> Graph Double ()
anchor Bottom = do
    attr "y" $ Postfix [jmacroE| 1 - `(cursor)` |]
    anchor Top

anchor Top = do
    attr "width" 1
    attr "height" $ Postfix cursor

anchor RightSide = do
    attr "x" $ Postfix [jmacroE| 1 - `(cursor)` |]
    anchor LeftSide

anchor LeftSide = do
    attr "width" $ Postfix cursor
    attr "height" 1

-- | Given a list of `Graph`s, renders them all side-by-side

split :: Orientation -> Graph a () -> Graph [a] ()
split ori graph =  
    selectAll "g" $ enter $ append "g" $ do
        attr "transform" size
        graph

    where
        size = case ori of
            Vertical -> do
                scale width 1
                translate index 0
            Horizontal -> do
                scale 1 width
                translate 0 index

        width = [jmacroE| 
            1 / (`(parent)`.datum() || `(target)`.data()).length 
        |]

-- | Arranges a 2D array as a grid, with proportional spacing.

grid :: Graph a () -> Graph [[a]] ()
grid = split Vertical . split Horizontal

-- | Pads a graph with some space.

pad :: Double -> Graph a () -> Graph a ()
pad x graph = append "g" $ do
    graph
    attr "transform" $ Const $ do
        translate (x / 2.0) (x / 2.0)
        scale (1.0 - x) (1.0 - x)

------------------------------------------------------------------------------
