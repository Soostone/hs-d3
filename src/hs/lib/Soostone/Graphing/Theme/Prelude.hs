------------------------------------------------------------------------------

-- | Prelude, the D3 Javascript needed to render a chart.  Should only be
--   needed once per rendering.

------------------------------------------------------------------------------

{-# LANGUAGE ExtendedDefaultRules  #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Soostone.Graphing.Theme.Prelude where

import Control.Lens

import Soostone.Graphing.D3
import Soostone.Graphing.Theme.Base

------------------------------------------------------------------------------

-- | The prelude for a new graph, this `Chart` initializes the svg node,
--   sets a D3 transform and group for the component graph elements.

withPrelude :: Theme a -> GraphT ThemeSt a () -> GraphT ThemeSt a ()
withPrelude theme graph = do
    userState .= themeSt theme
    select "body" $
    
        append "svg" $ do
            attr "viewBox" "0 0 1 1"
            attr "preserveAspectRatio" "none"
            attr "width" "100%"
            attr "height" "100%"
    
            append "defs" $
                themeDefs (defs theme)

            append "rect" $ do
                defaultSize
                attr "fill" backColor

            append "g" $ do
                attr "transform" $ Const $ do
                    translate 0.025 0.025
                    scale 0.95 0.95

                graph

------------------------------------------------------------------------------
