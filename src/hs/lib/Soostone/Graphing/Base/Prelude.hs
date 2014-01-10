------------------------------------------------------------------------------

-- | Prelude, the base Javascript needed to render a chart.  Should only be
--   needed once per rendering.

------------------------------------------------------------------------------

{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE QuasiQuotes           #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Soostone.Graphing.Base.Prelude where

import Language.Javascript.JMacro

import Soostone.Graphing.Base.Cursor
import Soostone.Graphing.Base.D3
import Soostone.Graphing.Base.Graph
import Soostone.Graphing.Base.Transform

------------------------------------------------------------------------------

-- | The prelude for a new graph, this `Graph` initializes the svg node,
--   sets a base transform and group for the component graph elements.

prelude :: Graph a ()
prelude = do
    setTarget svg
    attr "width" 640
    attr "height" 480
    setTarget g
    attr "transform" $ Const $ scale 640 480

    where
        svg = [jmacroE| d3.select("body").append("svg") |]
        g = [jmacroE| `(target)`.append("g") |]

------------------------------------------------------------------------------
