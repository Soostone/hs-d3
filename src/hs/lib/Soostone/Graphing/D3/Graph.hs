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

import Control.Applicative
import Control.Monad.State
import Language.Javascript.JMacro

------------------------------------------------------------------------------

-- | State record for use in the `Graph` monad.

data GraphState s = GraphState {
    _jstat   :: JStat -> JStat,
    _jCursor :: JExpr -> JExpr,
    _userState :: s,
    _varSeed :: Int
} deriving (Functor)

makeLenses ''GraphState

-- | Convenience method exposes a default GraphState for bootstraping

emptyState :: s -> GraphState s
emptyState s = GraphState {
    _jstat = id,
    _jCursor = id,
    _userState = s,
    _varSeed = 0
}

newVar :: GraphT s a String
newVar = ("d3_" ++) . show <$> (varSeed <<%= (+1))

-- | The core data structure representing a graph computation.

newtype GraphT s a b =
    Graph (State (GraphState s) b)
    deriving (Monad, Functor, MonadState (GraphState s))

type Graph = GraphT ()

runGraph :: GraphT s a b -> GraphState s -> (b, GraphState s)
runGraph (Graph s) = runState s

fanout :: GraphT s a b -> GraphT s c b
fanout x = do
    st <- get
    let (b, st') = runGraph x st
    put st'
    return b

--fanin :: GraphT s [a] b -> GraphT s a b
--fanin = modify . runGraph

-- | Given a `Graph`, extracts the `JStat` that this Graph would have
--   rendered.  Useful for compositional tools.
--   TODO there is probably a better way to do this.

extract :: GraphT s a b -> GraphT s a (b, JStat -> JStat)
extract gr = do
    oldSt <- get
    clear
    gr' <- gr
    newStat <- use jstat
    seed <- use varSeed
    put oldSt 
    varSeed .= seed
    return (gr', newStat)

-- | The compliment to extract, `insert` appends a snippet of `JStat`
--   to the state of the current `Graph`.

insert :: JStat -> GraphT s a ()
insert js = insertCont $ \cont -> [jmacro|
    `(js)`;
    `(cont)`;
|]

-- | The compliment to extract, `insert` appends a snippet of `JStat`
--   to the state of the current `Graph`.

insertCont :: (JStat -> JStat) -> GraphT s a ()
insertCont js = jstat %= (. js)

-- | Clears the current JStat - for internal user only.

clear :: GraphT s a ()
clear = jstat .= id

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
