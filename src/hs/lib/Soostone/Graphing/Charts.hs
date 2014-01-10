------------------------------------------------------------------------------

-- | Charts

------------------------------------------------------------------------------

{-# LANGUAGE QuasiQuotes #-}

module Soostone.Graphing.Charts where

import Language.Javascript.JMacro

import Soostone.Graphing.Base
import Soostone.Graphing.Shapes

------------------------------------------------------------------------------

-- | A simple bargraph, suitable for impressing your friends.

barGraph :: Graph [Double] ()
barGraph =
    split Vertical $ rect $ do
        attr "fill" toColor
        anchor Bottom

    where
        toColor = Postfix [jmacroE|
            `(colors)`[`(index)` % `(length colors)`]
        |]

        colors = 
            [ "#096975"
            , "#7DB31B"
            , "#CFCF0C"
            , "#EA552D"
            , "#DB2556" ]

-- | A vertically stacked bar graph.

stackedBarGraph :: Graph [[Double]] ()
stackedBarGraph = 
    split Horizontal $ pad 0.1 barGraph

-- | A grid of bar graphs.

gridBarGraph :: Graph [[[Double]]] ()
gridBarGraph = 
    grid $ pad 0.1 barGraph


------------------------------------------------------------------------------
