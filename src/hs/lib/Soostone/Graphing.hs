------------------------------------------------------------------------------

-- | This is the main interface, only file you shold need to import for REPL.
--   Also contains some test graphs for now.

------------------------------------------------------------------------------

module Soostone.Graphing(
    module Soostone.Graphing.Repl,
    module Soostone.Graphing.Shapes,
    module Soostone.Graphing.Charts,
    testGraph
) where

import qualified Data.List as L

import Soostone.Graphing.Repl
import Soostone.Graphing.Shapes
import Soostone.Graphing.Charts

------------------------------------------------------------------------------

testGraph :: IO ()
testGraph = 
    graph . barGraph . take 50 . L.cycle $
        [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8]

------------------------------------------------------------------------------
