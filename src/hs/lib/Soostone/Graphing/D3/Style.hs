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

instance ToCursor s Color where
    toCursor = return . toJExpr

gradient :: (ToCursor s a, ToCursor s b) =>
    Orientation -> String -> a -> b -> GraphT s c ()

gradient ori name from to =

    append "linearGradient" $ do
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

        append "stop" $ do
            attr "offset" "0%"
            attr "stop-color" from
            attr "stop-opacity" 1

        append "stop" $ do
            attr "offset" "100%"
            attr "stop-color" to
            attr "stop-opacity" 1

dropShadow :: String -> GraphT s a ()
dropShadow name =

    append "filter" $ do
        attr "id" name
        attr "height" "130%"

        append "feGaussianBlur" $ do
            attr "in" "SourceAlpha"
            attr "stdDeviation" 5
            attr "result" "blur"

        append "feOffset" $ do
            attr "in" "blur"
            attr "dx" 5
            attr "dyoffsetBlur" 5
            attr "result" "blur"

        append "feMerge" $ do

            append "feMergeNode" $
                attr "in" "offsetBlur"

            append "feMergeNode" $
                attr "in" "SourceGraphic"

------------------------------------------------------------------------------
