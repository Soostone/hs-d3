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
import Language.Javascript.JMacro

import Soostone.Graphing.Base
import Soostone.Graphing.Render

------------------------------------------------------------------------------

-- | Displays a graph in Chrome on OSX.

graph :: (ToJExpr a) => Graph a b -> a -> IO ()
graph gr dat = do
    writeGraph "lib/js/d3.v2.min.js" "test.html" gr dat
    void $ system "open -a Google\\ Chrome test.html"

-- | Writes a `Graph` to an HTML file.

writeGraph :: ToJExpr a => String -> String -> Graph a b -> a -> IO ()
writeGraph d3 file gr dat =
    writeFile file 
        . wrapHTML d3
        . renderGraph 
        . runGraph (prelude >> setData dat >> gr) 
        $ emptyState

-- | Appends the basic HTML necessary to bootstrap a graph into the browser.

wrapHTML :: String -> String -> String
wrapHTML d3 js = [qq|

    <script src="$d3"></script>
    <script> $js </script>

|]

------------------------------------------------------------------------------
