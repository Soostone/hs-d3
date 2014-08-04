------------------------------------------------------------------------------

-- | Layout primitives.

------------------------------------------------------------------------------

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes          #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Graphics.HSD3.Chart.Layout where

import Language.Javascript.JMacro

import Graphics.HSD3.Chart.Base
import Graphics.HSD3.D3

------------------------------------------------------------------------------

-- | A graph's anchor in it's current context.

data Anchor = Top | RightSide | Bottom | LeftSide

-- | Anchors the __target__

anchor :: Anchor -> Chart a ()
anchor Bottom = do
    attr "y" $ 1 - cursor
    anchor Top

anchor Top = do
    attr "width" 1
    attr "height" cursor

anchor RightSide = do
    attr "x" $ 1 - cursor
    anchor LeftSide

anchor LeftSide = do
    attr "width" cursor
    attr "height" 1

-- | Given a list of `Graph`s, renders them all side-by-side

split :: GraphT s a b -> Transform () -> GraphT s [a] JExpr
split graph size =
    selectAll "g" `with`
        bind cursor $
            enter `with` append "g" `with` do
                attr "transform" size
                graph

width :: JExpr
width = [jmacroE|
    1 / (`(parent)`.datum() || `(target)`.data()).length
|]

splitVertical :: GraphT s a b -> GraphT s [a] JExpr
splitVertical graph = split graph $ do
    scale width 1
    translate index 0

splitHorizontal :: GraphT s a b -> GraphT s [a] JExpr
splitHorizontal graph = split graph $ do
    scale 1 width
    translate 0 index

-- | Arranges a 2D array as a grid, with proportional spacing.

grid :: GraphT s a b -> GraphT s [[a]] JExpr
grid = splitVertical . splitHorizontal

-- | Pads a graph with some space.

pad :: Double -> GraphT s a b -> GraphT s a JExpr
pad x graph = append "g" `with` do
    _ <- graph
    attr "transform" $ do
        translate (x / 2) (x / 2)
        scale (1 - x) (1 - x)

------------------------------------------------------------------------------
