------------------------------------------------------------------------------

-- | REPL tools for debugging/interacting/developing `Graph`s

------------------------------------------------------------------------------

{-# LANGUAGE QuasiQuotes #-}

module Soostone.Graphing.Repl( 
    graph,
    writeGraph
) where

import Control.Monad.State
import System.Process
import Text.InterpolatedString.Perl6

import Soostone.Graphing.Base
import Soostone.Graphing.Render

------------------------------------------------------------------------------

-- | Displays a graph in Chrome on OSX.

graph :: Graph a -> IO ()
graph gr = do
    writeGraph "test.html" gr
    void $ system "open -a Google\\ Chrome test.html"

writeGraph :: String -> Graph a -> IO ()
writeGraph file gr =
    writeFile file 
        . wrapHTML 
        . renderGraph 
        . execState (prelude >> gr) 
        $ emptyState

-- | Appends the basic HTML necessary to bootstrap a graph into the browser.

wrapHTML :: String -> String
wrapHTML js = [qq|

    <script src="lib/js/d3.v2.min.js"></script>
    <script> $js </script>

|]

------------------------------------------------------------------------------
