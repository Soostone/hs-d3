------------------------------------------------------------------------------

-- | An implementation of an axis

------------------------------------------------------------------------------

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExtendedDefaultRules #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Soostone.Graphing.Chart.Axis where

import Language.Javascript.JMacro

import Soostone.Graphing.D3

------------------------------------------------------------------------------

axis :: GraphT s a JExpr
axis =
    append "g" `withT` do
        attr "class" "axis"

        dom <- clamp $ Post cursor

        scal <- linear `with` do
            domain dom
            range [0, 1]

        ts <- withTarget scal `withT` do
            nice
            ticks 10

        selectAll "line.y" `with`
            bindT ts $ enter `with` do

                _ <- append "text" `with` do
                    attr "x" 0
                    attr "y" scal
                    text $ Post $[jmacroE|
                        (`(dom)`[1] - `(cursor)`).toFixed(1)
                    |]
                    
                append "line" `with` do
                    attr "class" "y"
                    attr "x2" 1
                    attr "x1" 0
                    attr "y1" scal
                    attr "y2" scal
                    attr "stroke" "grey"
                    attr "stroke-width" "0.001"

        return scal
       
------------------------------------------------------------------------------
