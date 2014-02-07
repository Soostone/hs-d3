------------------------------------------------------------------------------

-- | The `Cursor` is an abstraction used to track how D3.hs should render
--   D3 parameters that are data dependent.  It accomplishes this through left
--   and right composition.

------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}

module Soostone.Graphing.D3.Cursor(
    Cursor( .. ),
    ToCursor( .. ),
 ) where

import Control.Lens
import Language.Javascript.JMacro

import Soostone.Graphing.D3.Graph
import Soostone.Graphing.D3.JMacro

------------------------------------------------------------------------------

-- | A data type for composable, data dependent subexpressions.  `Pre`
--   `Post` represent composition with other data transformations,
--   while `Const` is used for statically embedding a subexpression.

data Cursor = forall a. ToJExpr a => Post a
            | forall a. ToJExpr a => Pre a
            | forall a. ToJExpr a => Const a

-- | Encapsulates composition between cursors.  An a in `Graph` a `JExpr` can be
--   rendered as a data dependent parameter, and the implementation of how it
--   is rendered & composed is hidden in a's `ToCursor` instance.

class ToCursor s a b where
    toCursor :: a -> GraphT s b JExpr

instance (ToCursor a b c) => ToCursor a (a -> b) c where
    toCursor f = do
        theme <- use userState
        toCursor $ f theme

instance ToCursor s Integer b where
    toCursor = return . toJExpr

instance ToCursor s [Integer] b where
    toCursor = return . toJExpr

instance ToCursor s [Double] b where
    toCursor = return . toJExpr

instance ToCursor s Double b where
    toCursor = return . toJExpr

instance ToCursor s String b where
    toCursor = return . toJExpr

instance ToCursor s JExpr b where
    toCursor = return

instance ToCursor s Cursor b where

    toCursor (Const f) =
        return . toJExpr $ f

    toCursor (Post f) = do
        curs <- use jCursor
        return . curs . toJExpr $ f

    toCursor (Pre f) = do
        curs <- use jCursor
        return . (replace "__cursor__" . curs $ cursor) . toJExpr $ f

------------------------------------------------------------------------------
