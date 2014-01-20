------------------------------------------------------------------------------

-- | Theme

------------------------------------------------------------------------------

{-# LANGUAGE ExtendedDefaultRules      #-}
{-# LANGUAGE QuasiQuotes               #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Soostone.Graphing.Theme.Base where

import Control.Lens
import Language.Javascript.JMacro

import Soostone.Graphing.D3

------------------------------------------------------------------------------

class Default a where
    def :: a

data Theme a = Theme {
    defs    :: GraphT () a (),
    themeSt :: ThemeSt
}

data ThemeSt = ThemeSt {
    strokeColor :: Cursor,
    strokeWidth :: Cursor,
    foreColor   :: Cursor,
    backColor   :: Cursor
}

instance Default (Theme a) where
    def = Theme {
        defs = return (),
        themeSt = def
    }

instance Default ThemeSt where
    def = ThemeSt {
        foreColor = Const . Hex $ "#99B2B7",
        backColor = Const . Hex $ "white",
        strokeColor = Const . Hex $ "#000000",
        strokeWidth = Const 1
    }

themeDefs :: GraphT () a () -> GraphT ThemeSt a ()
themeDefs theme =
    insert $ runGraph theme (emptyState ()) ^. jstat

-- Utils

cycl :: ToJExpr a => JExpr -> [a] -> Cursor
cycl expr xs = Post [jmacroE|
    `(xs)`[`(expr)` % `(length xs)`]
|]

defaultSize :: GraphT s a ()
defaultSize = do
    attr "x" 0
    attr "y" 0
    attr "width" 1
    attr "height" 1

------------------------------------------------------------------------------
