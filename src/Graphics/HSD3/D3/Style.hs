------------------------------------------------------------------------------

-- | A DSL for generating SVG transform strings.

------------------------------------------------------------------------------

{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Graphics.HSD3.D3.Style where

import Language.Javascript.JMacro

import Graphics.HSD3.D3.Cursor
import Graphics.HSD3.D3.Graph
import Graphics.HSD3.D3.Scope
import Graphics.HSD3.D3.Selection

------------------------------------------------------------------------------

-- | Orientation type represents ... orientation!

data Orientation = Horizontal | Vertical

data Color = Hex String | URL String | RGB Int Int Int

instance ToJExpr Color where
    toJExpr (Hex c) = toJExpr c
    toJExpr (URL c) = toJExpr $ "url(#" ++ c ++ ")"
    toJExpr (RGB a b c) = toJExpr $
        "rgb(" ++ show a ++ "," ++ show b ++ "," ++ show c ++ ")"

instance ToCursor s a Color where
    toCursor = return . toJExpr

gradient :: (ToCursor s a b, ToCursor s a c) =>
    Orientation -> String -> b -> c -> GraphT s a JExpr

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
            attr "stop-opacity" (1 :: Double)

        append "stop" `with`do
            attr "offset" "100%"
            attr "stop-color" to
            attr "stop-opacity" (1 :: Double)

dropShadow :: String -> GraphT s a JExpr
dropShadow name =

    append "filter" `with` do
        attr "id" name
        attr "height" "130%"

        append "feGaussianBlur" `with` do
            attr "in" "SourceAlpha"
            attr "stdDeviation" (5 :: Double)
            attr "result" "blur"

        append "feOffset" `with` do
            attr "in" "blur"
            attr "dx" (5 :: Double)
            attr "dy" (5 :: Double)
            attr "result" "offsetBlur"

        append "feMerge" `with` do

            append "feMergeNode" `with`
                attr "in" "offsetBlur"

            append "feMergeNode" `with`
                attr "in" "SourceGraphic"

------------------------------------------------------------------------------
