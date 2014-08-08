------------------------------------------------------------------------------

-- | Prelude, the D3 Javascript needed to render a chart.  Should only be
--   needed once per rendering.

------------------------------------------------------------------------------

{-# LANGUAGE ExtendedDefaultRules #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Graphics.HSD3.Theme.Prelude where

import Control.Lens
import Control.Monad

import Graphics.HSD3.D3
import Graphics.HSD3.Theme.Base

------------------------------------------------------------------------------

-- | The prelude for a new graph, this `Chart` initializes the svg node,
--   sets a D3 transform and group for the component graph elements.

withPrelude :: Theme a -> GraphT ThemeSt a () -> GraphT ThemeSt a ()
withPrelude theme graph = void $ do
    userState .= themeSt theme
    select "body" `with`

        append "svg" `with` do
            attr "viewBox" "0 0 1 1"
            attr "preserveAspectRatio" "none"
            attr "width" "100%"
            attr "height" "100%"


            void $ append "defs" `with`
                themeDefs (defs theme)

            void $ append "rect" `with` do
                defaultSize
                attr "fill" backColor

            append "g" `with` do
                attr "transform" $ do
                    translate 0.025 0.025
                    scale 0.95 0.95

                graph

------------------------------------------------------------------------------
