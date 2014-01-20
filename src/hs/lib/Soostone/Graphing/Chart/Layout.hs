------------------------------------------------------------------------------

-- | Layout primitives.

------------------------------------------------------------------------------

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes          #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Soostone.Graphing.Chart.Layout where

import Language.Javascript.JMacro

import Soostone.Graphing.Chart.Base
import Soostone.Graphing.D3

------------------------------------------------------------------------------

-- | A graph's anchor in it's current context.

data Anchor = Top | RightSide | Bottom | LeftSide

-- | Anchors the __target__

anchor :: Anchor -> Chart a ()
anchor Bottom = do
    attr "y" $ Post $ 1 - cursor
    anchor Top

anchor Top = do
    attr "width" 1
    attr "height" $ Post cursor

anchor RightSide = do
    attr "x" $ Post $ 1 - cursor
    anchor LeftSide

anchor LeftSide = do
    attr "width" $ Post cursor
    attr "height" 1

-- | Given a list of `Graph`s, renders them all side-by-side

split :: GraphT s a () -> Transform () -> GraphT s [a] ()
split graph size =
    selectAll "g" $ bindData $ enter $ append "g" $ do
        attr "transform" size
        graph

width :: JExpr
width = [jmacroE|
    1 / (`(parent)`.datum() || `(target)`.data()).length
|]

splitVertical :: GraphT s a () -> GraphT s [a] ()
splitVertical graph = split graph $ do
    scale width 1
    translate index 0

splitHorizontal :: GraphT s a () -> GraphT s [a] ()
splitHorizontal graph = split graph $ do
    scale 1 width
    translate 0 index

-- | Arranges a 2D array as a grid, with proportional spacing.

grid :: GraphT s a () -> GraphT s [[a]] ()
grid = splitVertical . splitHorizontal

-- | Pads a graph with some space.

pad :: Double -> GraphT s a () -> GraphT s a ()
pad x graph = append "g" $ do
    graph
    attr "transform" $ Const $ do
        translate (x / 2) (x / 2)
        scale (1 - x) (1 - x)

------------------------------------------------------------------------------
