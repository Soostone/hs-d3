------------------------------------------------------------------------------

-- | This is the main interface, only file you shold need to import for REPL

------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE QuasiQuotes            #-}

module Soostone.Graphing.D3.Render(
    Render( .. ),
    renderGraph
) where

import Control.Lens
import Data.Monoid
import Language.Javascript.JMacro
import Text.PrettyPrint.Leijen.Text hiding (group, width, (<$>))
import Text.Regex

import Soostone.Graphing.D3.Graph
import Soostone.Graphing.D3.JMacro

------------------------------------------------------------------------------

class Render b where
    render :: ToJExpr a => a -> b a () -> String

instance Render (GraphT ()) where
    render = renderGraph ()

-- | Renders a `GraphT` s a () as a `String`.

renderGraph :: ToJExpr a => s -> a -> GraphT s a () -> String
renderGraph st dat graph =
    renderGraphState
    . snd
    . runGraph (setData dat >> graph)
    $ emptyState st

-- | Renders a GraphState as a string.

renderGraphState :: GraphState s -> String
renderGraphState =
    renderText
        . replace "__target__" (jsv "d3")
        . replace "__index__" 0
        . replace "__group__" 0
        . replace "__datum__" (jsv "__cursor__")
        . ($ mempty)
        . view jstat

-- | Renders a `JExpr` as a string.

renderText :: JStat -> String
renderText = compress . show . renderPretty 1.0 500 . renderJs . onload

compress :: String -> String
compress = flip (subRegex (mkRegexWithOpts "var (jmId_[0-9]+);[ \\t\n]*jmId_[0-9]+" True True)) "var \\1"

-- | Convenience method for attaching behavior to the browser window's
--   onload method.

onload :: JStat -> JStat
onload js = [jmacro|
    window.onload = function () { 
        `(js)`; 

        var svg  = d3.selectAll("svg");
        var text = d3.selectAll("text");

        window.onresize = function() {
            var height = parseFloat(svg.style("height"));
            var width  = parseFloat(svg.style("width"));
            var scalar = 12 / height;
            text.style("font-size", scalar).attr("transform", "scale(" + (height / width) + ", 1)");
        };
        window.onresize();
    };
|]

------------------------------------------------------------------------------
