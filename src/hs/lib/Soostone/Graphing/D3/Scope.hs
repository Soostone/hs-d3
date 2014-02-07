------------------------------------------------------------------------------

-- | The `Cursor` is an abstraction used to track how D3.hs should render
--   D3 parameters that are data dependent.  It accomplishes this through left
--   and right composition.

------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE QuasiQuotes               #-}

module Soostone.Graphing.D3.Scope where

import Control.Lens
import Data.Monoid
import Language.Javascript.JMacro

import Soostone.Graphing.D3.Cursor
import Soostone.Graphing.D3.Graph
import Soostone.Graphing.D3.JMacro

------------------------------------------------------------------------------

-- | Changes the definition of __target__ for the scope of `gr`.  Since
--   the new target is data dependent, it relies on the cursor package.

withTarget :: (ToCursor s b a) => b -> GraphT s a JExpr
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
    (_, inner) <- extract ext
    insert [jmacro|
        `(replace "__target__" expr $ inner mempty)`;
    |]

    return expr

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

scaleCursor :: JExpr -> GraphT s a c -> GraphT s a c
scaleCursor r graph = do
    oldCursor <- use jCursor
    -- c <- toCursor r
    jCursor %= (. (replace "__cursor__" [jmacroE| `(r)`(`(cursor)`) |]))
    b <- graph
    jCursor .= oldCursor
    return b

-- | Scopes the `jCursor` property of the `GraphState` in a `Graph`.

scopeCursor :: GraphT s a b -> GraphT s a b
scopeCursor graph = do
    oldCursor <- use jCursor
    jCursor .= callback
    b <- graph
    jCursor .= oldCursor
    return b

callback :: JExpr -> JExpr
callback a = [jmacroE|
    function(x, y, z) {
        return `(sanitize x y z $ a)`;
    }
|] where
    sanitize x y z = replace "__cursor__" x
        . replace "__index__" y
        . replace "__group__" z

------------------------------------------------------------------------------
