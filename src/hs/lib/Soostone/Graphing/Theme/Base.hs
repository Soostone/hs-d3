------------------------------------------------------------------------------

-- | Theme

------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExtendedDefaultRules      #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RankNTypes                #-}

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

data Box = forall a. ToJExpr a => Box a

instance ToCursor ThemeSt a Box where
    toCursor (Box x) = toCursor . toJExpr $ x

data ThemeSt = ThemeSt {
    strokeColor :: Box,
    strokeWidth :: Box,
    foreColor   :: Box,
    backColor   :: Box
}

instance Default (Theme a) where
    def = Theme {
        defs = return (),
        themeSt = def
    }

instance Default ThemeSt where
    def = ThemeSt {
        foreColor   = Box $ Hex "#99B2B7",
        backColor   = Box $ Hex "white",
        strokeColor = Box $ Hex "#000000",
        strokeWidth = Box (1 :: Integer)
    }

themeDefs :: GraphT () a () -> GraphT ThemeSt a ()
themeDefs theme =
    insertCont $ snd (runGraph theme (emptyState ())) ^. jstat

-- Utils

cycl :: ToJExpr a => JExpr -> [a] -> JExpr
cycl expr xs = [jmacroE|
    `(xs)`[`(expr)` % `(length xs)`]
|]

defaultSize :: GraphT s a ()
defaultSize = do
    attr "x" 0
    attr "y" 0
    attr "width" 1
    attr "height" 1

------------------------------------------------------------------------------
