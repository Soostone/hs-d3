------------------------------------------------------------------------------

-- | Core library components.  Most anything that touches raw JMacro should
--   go in this module for now.

------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}

module Soostone.Graphing.Base where

import Control.Lens
import Control.Monad.State
import Data.Monoid
import Language.Javascript.JMacro

------------------------------------------------------------------------------

-- | State record for use in the `Graph` monad.

data GraphState = GraphState {
    _jstat :: JStat
}

makeLenses ''GraphState

-- | Convenience method exposes a default GraphState for bootstraping

emptyState :: GraphState
emptyState = GraphState {
    _jstat = mempty
} 

-- | The core data structure representing a graph computation.

type Graph a = State GraphState a

-- | The prelude for a new graph, this `Graph` initializes the svg node,
--   sets a base transform and group for the component graph elements.

prelude :: Graph ()
prelude = do
    setTarget [jmacroE| d3.select("body").append("svg") |]
    attr "width" (640 :: Int)
    attr "height" (480 :: Int)
    setTarget [jmacroE| __target__.append("g") |]
    attr "transform" "scale(640, 480)"

-- | Convenience method for Defs

setDefs :: ToJExpr a => a -> Graph ()
setDefs x = insert [jmacro|
    var !__defs__ = `(x)`;
|]   

-- | Convenience method for attaching behavior to the browser window's
--   onload method.

onload :: JStat -> JStat
onload js = [jmacro|
    window.onload = function () { `(js)`; };
|]

-- | Changes the definition of __target__ for the scope of `gr`.

withTarget :: JExpr -> Graph () -> Graph ()
withTarget st gr = do
    inner <- extract gr
    insert [jmacro|
        (function(x) {
            var !__target__ = x;
            `(inner)`;
        })(`(st)`);
    |]

-- | Creates a new SVG node of a specific type

node :: String -> JExpr
node el = [jmacroE| __target__.append(`(el)`) |]

-- | Sets an attribute on the SVG __target__

attr :: ToJExpr a => String -> a -> Graph ()
attr target prop = 
    insert [jmacro|
        __target__.attr(`(target)`, `(prop)`);
    |]

-- | Given a `Graph`, extracts the `JStat` that this Graph would have 
--   rendered.  Useful for compositional tools.
--   TODO there is probably a better way to do this.

extract :: Graph () -> Graph JStat
extract gr = do
    oldSt <- get
    jstat .= mempty
    gr
    newStat <- use jstat
    put oldSt
    return newStat

-- | The compliment to extract, `insert` appends a snippet of `JStat`
--   to the state of the current `Graph`.

insert :: JStat -> Graph ()
insert = (jstat <>=)

-- | Convenience method for target

setTarget :: ToJExpr a => a -> Graph ()
setTarget x = insert [jmacro|
    var !__target__ = `(x)`;
|]


------------------------------------------------------------------------------
