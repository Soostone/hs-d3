------------------------------------------------------------------------------

-- | Charts

------------------------------------------------------------------------------

module Soostone.Graphing.Charts where

import Text.InterpolatedString.Perl6

import qualified Data.List as L

import Soostone.Graphing.Base
import Soostone.Graphing.Shapes
import Soostone.Graphing.Utils

------------------------------------------------------------------------------

barGraph :: [Double] -> Graph ()
barGraph heights =
    split Vertical (bar `mapI` heights )

    where
        colors = L.cycle ["red", "green", "blue"]
        bar h i = rect $ do
            attr "fill" (colors !! i)
            anchor Bottom h

-- | A vertically stacked bar graph

stackedBarGraph :: [[Double]] -> Graph ()
stackedBarGraph xs = 
    split Horizontal (fmap (pad 0.9 . barGraph) xs)

------------------------------------------------------------------------------
