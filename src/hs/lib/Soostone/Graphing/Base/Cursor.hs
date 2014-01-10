------------------------------------------------------------------------------

-- | The `Cursor` is an abstraction used to track how D3.hs should render
--   D3 parameters that are data dependent.  It accomplishes this through left
--   and right composition.

------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Soostone.Graphing.Base.Cursor(
    Cursor( .. ),
    withTarget,
    ToCursor( .. ),
    scopeCursor
) where

import Control.Lens
import Language.Javascript.JMacro

import Soostone.Graphing.Base.Graph
import Soostone.Graphing.Base.JMacro

------------------------------------------------------------------------------

-- | A data type for composable, data dependent subexpressions.  `Prefix`
--   `Postfix` represent composition with other data transformations,
--   while `Const` is used for statically embedding a subexpression.

data Cursor a = Postfix a | Prefix a | Const a

-- | Changes the definition of __target__ for the scope of `gr`.  Since
--   the new target is data dependent, it relies on the cursor package.

withTarget :: ToCursor b => b -> Graph a () -> Graph a ()
withTarget st gr = do
    inner <- extract gr
    st'   <- toCursor st
    insert [jmacro|
        var x = `(st')`;
        `(replace "__target__" x inner)`;
    |]

-- | Encapsulates composition between cursors.  An a in `Graph` a `JExpr` can be
--   rendered as a data dependent parameter, and the implementation of how it
--   is rendered & composed is hidden in a's `ToCursor` instance.

class ToCursor a where
    toCursor :: a -> Graph b JExpr

instance ToCursor JExpr where
    toCursor = return

instance ToCursor Int where
    toCursor = return . toJExpr

instance ToCursor String where
    toCursor = return . toJExpr

instance ToCursor Double where
    toCursor = return . toJExpr

instance ToCursor (Cursor JExpr) where

    toCursor (Const f) =
        return f

    toCursor (Postfix f) = do
        curs <- use jCursor
        return $ curs f

    toCursor (Prefix f) = do
        curs <- use jCursor
        return $ replace "__cursor__" (curs $ jsv "__cursor__") f

-- | Scopes the `jCursor` property of the `GraphState` in a `Graph`.

scopeCursor :: Graph a () -> Graph a ()
scopeCursor graph = do
    oldCursor <- use jCursor
    jCursor .= callback
    graph
    jCursor .= oldCursor

callback :: JExpr -> JExpr
callback x = [jmacroE|
    function(y, z) {
        return `(replace "__cursor__" y $ replace "__index__" z x)`;
    }
|]

------------------------------------------------------------------------------
