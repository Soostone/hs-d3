------------------------------------------------------------------------------

-- | Core library components.  Most anything that touches raw JMacro should
--   go in this module for now.

------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving                 #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Soostone.Graphing.Base.Graph where

import Control.Lens hiding (index, transform)
import Control.Monad.State
import Data.Monoid
import Language.Javascript.JMacro

------------------------------------------------------------------------------

-- | State record for use in the `Graph` monad.

data GraphState = GraphState {
    _jstat  :: JStat,
    _jCursor :: JExpr -> JExpr
}

makeLenses ''GraphState

-- | Convenience method exposes a default GraphState for bootstraping

emptyState :: GraphState
emptyState = GraphState {
    _jstat  = mempty,
    _jCursor = id
}

-- | The core data structure representing a graph computation.

newtype Graph a b =
    Graph (State GraphState b) 
    deriving (Monad, Functor, MonadState GraphState)

runGraph :: Graph a b -> GraphState -> GraphState
runGraph (Graph s) = execState s

fanout :: Graph a () -> Graph [a] ()
fanout = modify . runGraph

-- | Given a `Graph`, extracts the `JStat` that this Graph would have
--   rendered.  Useful for compositional tools.
--   TODO there is probably a better way to do this.

extract :: Graph a () -> Graph a JStat
extract gr = do
    oldSt <- get
    clear
    gr
    newStat <- use jstat
    put oldSt
    return newStat

-- | The compliment to extract, `insert` appends a snippet of `JStat`
--   to the state of the current `Graph`.

insert :: JStat -> Graph a ()
insert = (jstat <>=)

-- | Clears the current JStat - for internal user only.

clear :: Graph a ()
clear = jstat .= mempty

-- | Convenience method for target

setTarget :: ToJExpr a => a -> Graph b ()
setTarget x = insert [jmacro|
    var !__target__ = `(x)`;
|]

setData :: ToJExpr a => a -> Graph a ()
setData x = insert [jmacro| 
    var !__cursor__ = `(x)`;
|]

cursor :: JExpr
cursor = jsv "__cursor__"

target :: JExpr
target = jsv "__target__"

index :: JExpr
index = jsv "__index__"

parent :: JExpr
parent = [jmacroE| d3.select(this.parentNode) |]

------------------------------------------------------------------------------
