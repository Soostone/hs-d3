------------------------------------------------------------------------------

-- | The `Cursor` is an abstraction used to track how D3.hs should render
--   D3 parameters that are data dependent.  It accomplishes this through left
--   and right composition.

------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE QuasiQuotes               #-}

module Soostone.Graphing.D3.Cursor(
    Cursor( .. ),
    ToCursor( .. ),
    scopeCursor,
    withTarget
) where

import Control.Lens
import Language.Javascript.JMacro

import Soostone.Graphing.D3.Graph
import Soostone.Graphing.D3.JMacro

------------------------------------------------------------------------------

-- | A data type for composable, data dependent subexpressions.  `Prefix`
--   `Postfix` represent composition with other data transformations,
--   while `Const` is used for statically embedding a subexpression.

data Cursor = forall a. ToJExpr a => Postfix a
            | forall a. ToJExpr a => Prefix a
            | forall a. ToJExpr a => Const a

-- | Encapsulates composition between cursors.  An a in `Graph` a `JExpr` can be
--   rendered as a data dependent parameter, and the implementation of how it
--   is rendered & composed is hidden in a's `ToCursor` instance.

class ToCursor s a where
    toCursor :: a -> GraphT s b JExpr

instance (ToCursor a b) => ToCursor a (a -> b) where
    toCursor f = do
        theme <- use userState
        toCursor $ f theme

instance ToCursor s Integer where
    toCursor = return . toJExpr

instance ToCursor s Double where
    toCursor = return . toJExpr

instance ToCursor s String where
    toCursor = return . toJExpr

instance ToCursor s JExpr where
    toCursor = return

instance ToCursor s Cursor where

    toCursor (Const f) =
        return . toJExpr $ f

    toCursor (Postfix f) = do
        curs <- use jCursor
        return . curs . toJExpr $ f

    toCursor (Prefix f) = do
        curs <- use jCursor
        return . (replace "__cursor__" . curs . jsv $ "__cursor__") . toJExpr $ f

-- | Changes the definition of __target__ for the scope of `gr`.  Since
--   the new target is data dependent, it relies on the cursor package.

withTarget :: (ToCursor s b) => b -> GraphT s a () -> GraphT s a ()
withTarget st gr = do
    inner <- extract gr
    st'   <- toCursor st
    insert [jmacro|
        var x = `(st')`;
        `(replace "__target__" x inner)`;
    |]
-- | Scopes the `jCursor` property of the `GraphState` in a `Graph`.

scopeCursor :: GraphT s a () -> GraphT s a ()
scopeCursor graph = do
    oldCursor <- use jCursor
    jCursor .= callback
    graph
    jCursor .= oldCursor

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
