------------------------------------------------------------------------------

-- | The `Cursor` is an abstraction used to track how D3.hs should render
--   D3 parameters that are data dependent.  It accomplishes this through left
--   and right composition.

------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE QuasiQuotes               #-}

module Soostone.Graphing.D3.Cursor(
    ToCursor( .. ),
 ) where

import Control.Lens
import Language.Javascript.JMacro as J

import Soostone.Graphing.D3.Graph
import Soostone.Graphing.D3.JMacro

------------------------------------------------------------------------------

-- | Encapsulates composition between cursors.  An a in `Graph` a `JExpr` can be
--   rendered as a data dependent parameter, and the implementation of how it
--   is rendered & composed is hidden in a's `ToCursor` instance.

class ToCursor s a b where
    toCursor :: b -> GraphT s a JExpr
    default toCursor :: (ToJExpr b) => b -> GraphT s a JExpr
    toCursor f = do
        curs <- use jCursor
        joined <- use jJoined
        let ex = curs $ toJExpr f
        return $ case (joined, count "__cursor__" ex + count "__group__" ex + count "__index__" ex) of
            (_, 0)     -> ex
            (False, _) -> ex
            (True, _)  -> callback ex

instance ToCursor s a Integer
instance ToCursor s a [Integer]
instance ToCursor s a [Double]
instance ToCursor s a Double
instance ToCursor s a String
instance ToCursor s a JExpr

instance (ToCursor a b c) => ToCursor a b (a -> c) where
    toCursor f = do
        theme <- use userState
        toCursor $ f theme

callback :: JExpr -> JExpr
callback a = [jmacroE|

    function(x, y, z) {
        return `(sanitize x y z $ a)`;
    }

|] where
    sanitize x y z = replace "__cursor__" x
        . replace "__index__" y
        . replace "__group__" z

count :: JMacro a => String -> a -> Int
count s =
    f . jtoGADT . jsSaturate Nothing
    where
        f :: JMGadt a -> Int
        f (JMGExpr (ValExpr (JVar (StrI z)))) | z == s = 1
        f z = J.composOpFold 0 (+) f z

------------------------------------------------------------------------------
