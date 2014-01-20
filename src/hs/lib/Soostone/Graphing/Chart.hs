------------------------------------------------------------------------------

-- | Charts

------------------------------------------------------------------------------

{-# LANGUAGE ExtendedDefaultRules  #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Soostone.Graphing.Chart(
    module Soostone.Graphing.Chart.Base,
    module Soostone.Graphing.Chart.Element,
    module Soostone.Graphing.Chart.Layout,
    barGraph,
    stackedBarGraph,
    gridBarGraph
) where

import Soostone.Graphing.Chart.Base
import Soostone.Graphing.Chart.Element
import Soostone.Graphing.Chart.Layout
import Soostone.Graphing.D3
import Soostone.Graphing.Theme

------------------------------------------------------------------------------

-- | A simple bargraph, suitable for impressing your friends.

barGraph :: Chart [Double] ()
barGraph = do

    axis

    append "g" $ do

        attr "transform" $ Const $ do
            translate 0.05 0.0
            scale 0.95 1.0

        splitVertical $ rect $ do
            attr "fill" foreColor
            attr "stroke" strokeColor
            attr "stroke-width" strokeWidth
            attr "vector-effect" "non-scaling-stroke"
            anchor Bottom

-- | A vertically stacked bar graph.

stackedBarGraph :: Chart [[Double]] ()
stackedBarGraph = 
    splitHorizontal $ pad 0.05 barGraph

-- | A grid of bar graphs.

gridBarGraph :: Chart [[[Double]]] ()
gridBarGraph = 
    grid $ pad 0.05 barGraph

------------------------------------------------------------------------------
