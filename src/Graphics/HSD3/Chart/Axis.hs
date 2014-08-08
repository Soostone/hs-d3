------------------------------------------------------------------------------

-- | An implementation of an xAxis

------------------------------------------------------------------------------

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes          #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Graphics.HSD3.Chart.Axis where

import Control.Lens
import Control.Monad
import Language.Javascript.JMacro

import Graphics.HSD3.D3

------------------------------------------------------------------------------

yAxis :: GraphT s a JExpr
yAxis =
    append "g" `withT` do
        attr "class" "yAxis"

        dom <- clamp cursor

        scal <- linear `with` do
            domain dom
            range ([0, 1] :: [Double])

        ts <- withTarget scal `withT` do
            nice
            ticks (10 :: Integer)

        void $ selectAll "line.x" `with`
            bind ts $ enter `with` do

                jCursor .= id

                _ <- append "text" `with` do
                    attr "x" scal
                    attr "y" (0 :: Double)
                    text $ [jmacroE|
                        (`(dom)`[1] - `(cursor)`).toFixed(1)
                    |]

                append "line" `with` do
                    attr "class" "x"
                    attr "x2" scal
                    attr "x1" scal
                    attr "y1" (0 :: Double)
                    attr "y2" (1 :: Double)
                    attr "stroke" "grey"
                    attr "stroke-width" "0.001"

        return [jmacroE| `(scal)`(`(cursor)`) |]

xAxis :: GraphT s a JExpr
xAxis =
    append "g" `withT` do
        attr "class" "xAxis"

        dom <- clamp cursor

        scal <- linear `with` do
            domain dom
            range ([0, 1] :: [Double])

        ts <- withTarget scal `withT` do
            nice
            ticks (10 :: Integer)

        void $ selectAll "line.y" `with`
            bind ts $ enter `with` do

                jCursor .= id

                _ <- append "text" `with` do
                    attr "x" (0 :: Double)
                    attr "y" scal
                    text $ [jmacroE|
                        (`(dom)`[1] - `(cursor)`).toFixed(1)
                    |]

                append "line" `with` do
                    attr "class" "y"
                    attr "x2" (1 :: Double)
                    attr "x1" (0 :: Double)
                    attr "y1" scal
                    attr "y2" scal
                    attr "stroke" "grey"
                    attr "stroke-width" "0.001"

        return [jmacroE| `(scal)`(`(cursor)`) |]

------------------------------------------------------------------------------
