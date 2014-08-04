------------------------------------------------------------------------------

-- | Charts

------------------------------------------------------------------------------

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes          #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Graphics.HSD3.Chart
  ( module Graphics.HSD3.Chart.Base
  , module Graphics.HSD3.Chart.Element
  , module Graphics.HSD3.Chart.Layout
  , barGraph
  , stackedBarGraph
  , gridBarGraph
  , coloredBarGraph
  , Bar( ..)
  ) where


------------------------------------------------------------------------------
import Control.Lens
import Control.Monad
import Language.Javascript.JMacro
------------------------------------------------------------------------------
import Graphics.HSD3.Chart.Axis
import Graphics.HSD3.Chart.Base
import Graphics.HSD3.Chart.Element
import Graphics.HSD3.Chart.Layout
import Graphics.HSD3.D3
import Graphics.HSD3.D3.JMacro
import Graphics.HSD3.Theme
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | A simple bargraph, suitable for impressing your friends.
barGraph :: Chart [Double] ()
barGraph = void $ do

    sc <- xAxis

    append "g" `with` do

        attr "transform" $ do
            translate 0.05 0.0
            scale 0.95 1.0

        splitVertical $ rect `with` withCursor sc $ do
            attr "fill" foreColor
            attr "stroke" strokeColor
            attr "stroke-width" strokeWidth
            attr "vector-effect" "non-scaling-stroke"
            anchor Bottom


------------------------------------------------------------------------------
data Bar = Bar { value :: Double, color :: String }


------------------------------------------------------------------------------
-- | TODO this should be auto-generated for records somehow
instance ToJExpr Bar where
    toJExpr b = [jmacroE|

            { value: `(value b)`, color: `(color b)` }

        |]


------------------------------------------------------------------------------
getValue :: JExpr
getValue =
    [jmacroE| `(cursor)`.value |]


------------------------------------------------------------------------------
getColor :: JExpr
getColor =
    [jmacroE| `(cursor)`.color |]


------------------------------------------------------------------------------
-- | A colored bar graph designed to work with metadata records
coloredBarGraph :: Chart [Bar] ()
coloredBarGraph = void $ do

    sc <- withCursor getValue xAxis

    append "g" `with` do

        attr "transform" $ do
            translate 0.05 0.0
            scale 0.95 1.0

        splitVertical $ rect `with` do
            withCursor getColor $
                attr "fill" cursor

            withCursor getValue $ withCursor sc  $ do
                attr "stroke" strokeColor
                attr "stroke-width" strokeWidth
                attr "vector-effect" "non-scaling-stroke"
                anchor Bottom


------------------------------------------------------------------------------
-- | A vertically stacked bar graph.
stackedBarGraph :: Chart [[Double]] ()
stackedBarGraph =
    void $ splitHorizontal $ pad 0.05 barGraph


------------------------------------------------------------------------------
-- | A grid of bar graphs.
gridBarGraph :: Chart [[[Double]]] ()
gridBarGraph =
    void $ grid $ pad 0.05 barGraph

