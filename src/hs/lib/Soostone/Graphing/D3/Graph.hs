------------------------------------------------------------------------------

-- | Core library components.  Most anything that touches raw JMacro should
--   go in this module for now.

------------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}

module Soostone.Graphing.D3.Graph where

import Control.Lens hiding (index, transform)

import Control.Monad.State
import Data.Monoid
import Language.Javascript.JMacro

------------------------------------------------------------------------------

-- | State record for use in the `Graph` monad.

data GraphState s = GraphState {
    _jstat   :: JStat,
    _jCursor :: JExpr -> JExpr,
    _userState :: s
} deriving (Functor)

makeLenses ''GraphState

-- | Convenience method exposes a default GraphState for bootstraping

emptyState :: s -> GraphState s
emptyState s = GraphState {
    _jstat = mempty,
    _jCursor = id,
    _userState = s
}

-- | The core data structure representing a graph computation.

newtype GraphT s a b =
    Graph (State (GraphState s) b)
    deriving (Monad, Functor, MonadState (GraphState s))

type Graph = GraphT ()

runGraph :: GraphT s a b -> GraphState s -> GraphState s
runGraph (Graph s) = execState s

fanout :: GraphT s a () -> GraphT s [a] ()
fanout = modify . runGraph

-- | Given a `Graph`, extracts the `JStat` that this Graph would have
--   rendered.  Useful for compositional tools.
--   TODO there is probably a better way to do this.

extract :: GraphT s a () -> GraphT s a JStat
extract gr = do
    oldSt <- get
    clear
    gr
    newStat <- use jstat
    put oldSt
    return newStat

-- | The compliment to extract, `insert` appends a snippet of `JStat`
--   to the state of the current `Graph`.

insert :: JStat -> GraphT s a ()
insert = (jstat <>=)

-- | Clears the current JStat - for internal user only.

clear :: GraphT s a ()
clear = jstat .= mempty

-- | Convenience method for target

setTarget :: ToJExpr a => a -> GraphT s b ()
setTarget x = insert [jmacro|
    var !__target__ = `(x)`;
|]

setData :: ToJExpr a => a -> GraphT s a ()
setData x = insert [jmacro|
    var !__cursor__ = `(x)`;
|]

cursor :: JExpr
cursor = jsv "__cursor__"

target :: JExpr
target = jsv "__target__"

index :: JExpr
index = jsv "__index__"

group :: JExpr
group = jsv "__group__"

parent :: JExpr
parent = [jmacroE| d3.select(this.parentNode) |]

------------------------------------------------------------------------------
