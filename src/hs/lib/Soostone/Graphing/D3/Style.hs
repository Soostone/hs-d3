------------------------------------------------------------------------------

-- | A DSL for generating SVG transform strings.

------------------------------------------------------------------------------

{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Soostone.Graphing.D3.Style where

import Language.Javascript.JMacro

import Soostone.Graphing.D3.Cursor
import Soostone.Graphing.D3.Graph
import Soostone.Graphing.D3.Scope
import Soostone.Graphing.D3.Selection

------------------------------------------------------------------------------

-- | Orientation type represents ... orientation!

data Orientation = Horizontal | Vertical

data Color = Hex String | URL String | RGB Int Int Int

instance ToJExpr Color where
    toJExpr (Hex c) = toJExpr c
    toJExpr (URL c) = toJExpr $ "url(#" ++ c ++ ")"
    toJExpr (RGB a b c) = toJExpr $
        "rgb(" ++ show a ++ "," ++ show b ++ "," ++ show c ++ ")"

instance ToCursor s Color b where
    toCursor = return . toJExpr

gradient :: (ToCursor s a c, ToCursor s b c) =>
    Orientation -> String -> a -> b -> GraphT s c JExpr

gradient ori name from to =

    append "linearGradient" `with` do
        attr "id" name
        attr "x1" "0%"
        attr "y1" "0%"

        case ori of
            Vertical -> do
                attr "x2" "0%"
                attr "y2" "100%"
            Horizontal -> do
                attr "x2" "100%"
                attr "y2" "0%"

        attr "spreadMethod" "pad"

        append "stop" `with` do
            attr "offset" "0%"
            attr "stop-color" from
            attr "stop-opacity" 1

        append "stop" `with`do
            attr "offset" "100%"
            attr "stop-color" to
            attr "stop-opacity" 1

dropShadow :: String -> GraphT s a JExpr
dropShadow name =

    append "filter" `with` do
        attr "id" name
        attr "height" "130%"

        append "feGaussianBlur" `with` do
            attr "in" "SourceAlpha"
            attr "stdDeviation" 5
            attr "result" "blur"

        append "feOffset" `with` do
            attr "in" "blur"
            attr "dx" 5
            attr "dy" 5
            attr "result" "offsetBlur"

        append "feMerge" `with` do

            append "feMergeNode" `with`
                attr "in" "offsetBlur"

            append "feMergeNode" `with`
                attr "in" "SourceGraphic"

------------------------------------------------------------------------------
