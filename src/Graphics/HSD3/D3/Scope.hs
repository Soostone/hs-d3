------------------------------------------------------------------------------

-- | The `Cursor` is an abstraction used to track how D3.hs should render
--   D3 parameters that are data dependent.  It accomplishes this through left
--   and right composition.

------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE QuasiQuotes               #-}

module Graphics.HSD3.D3.Scope where

import Control.Lens
import Data.Monoid
import Language.Javascript.JMacro

import Graphics.HSD3.D3.Cursor
import Graphics.HSD3.D3.Graph
import Graphics.HSD3.D3.JMacro

------------------------------------------------------------------------------

-- | Changes the definition of __target__ for the scope of `gr`.  Since
--   the new target is data dependent, it relies on the cursor package.

withTarget :: JExpr -> GraphT s a JExpr
withTarget st = do
    st' <- toCursor st
    var <- newVar
    insertCont $ \cont -> [jmacro|
        var x = `(st')`;
        `(replace var x cont)`;
    |]
    return $ jsv var

with :: GraphT s a JExpr -> GraphT s a b -> GraphT s a JExpr
with node ext = do
    expr <- node
    joined <- use jJoined
    (_, inner) <- extract ext
    let f = if joined then replace "__datum__" xx else id
    insert [jmacro|
        `(replace "__target__" expr $ f $ inner mempty)`;
    |]

    return expr

    where xx = [jmacroE| `(target)`.datum() |]

infixr 0 `with`

withT :: JMacro b => GraphT s a JExpr -> GraphT s a b -> GraphT s a b
withT node ext =  do
    expr <- node
    (ex, inner) <- extract ext
    x <- newVar
    insertCont $ replace x (jsv "__target__")
        . replace "__target__" expr
        . inner
        . replace "__target__" (jsv x)

    return $ replace "__target__" expr ex

infixr 0 `withT`

withCursor :: JExpr -> GraphT s a c -> GraphT s a c
withCursor r graph = do
    oldCursor <- use jCursor
    jCursor %= (. (replace "__cursor__" r))
    b <- graph
    jCursor .= oldCursor
    return b

-- | Scopes the `jCursor` property of the `GraphState` in a `Graph`.

scopeCursor :: GraphT s a b -> GraphT s a b
scopeCursor graph = do
    oldCursor <- use jCursor
    joined <- use jJoined
    jJoined .= True
    b <- graph
    jCursor .= oldCursor
    jJoined .= joined
    return b

------------------------------------------------------------------------------
