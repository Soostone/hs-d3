------------------------------------------------------------------------------

-- | This is the main interface, only file you shold need to import for REPL.
--   Also contains some test graphs for now.

------------------------------------------------------------------------------

module Soostone.Graphing(
    module Soostone.Graphing.Repl,
    module Soostone.Graphing.Chart,
    testGraph,
    testGraph2,
    testGraph3
) where

import qualified Data.List as L

import Soostone.Graphing.Repl
import Soostone.Graphing.Chart
import Soostone.Graphing.Theme

------------------------------------------------------------------------------

testGraph :: IO ()
testGraph = 
    graph dat (ThemeChart banaaniTheme barGraph)

    where
        dat :: [Double]
        dat = take 50 . L.cycle $
            [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8]

testGraph2 :: IO ()
testGraph2 = 
    graph dat (ThemeChart banaaniTheme stackedBarGraph)

    where
        dat :: [[Double]]
        dat = replicate 5 . take 50 . L.cycle $
            [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8]

testGraph3 :: IO ()
testGraph3 =
    graph dat (ThemeChart banaaniTheme stackedBarGraph)

    where 
        dat :: [[Double]]
        dat = [
            take 50 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
            take 50 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
            take 50 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
            take 50 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
            take 50 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8]
          ]

------------------------------------------------------------------------------
