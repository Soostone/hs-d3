------------------------------------------------------------------------------

-- | Chart Base

------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Graphics.HSD3.Chart.Base where

import Graphics.HSD3.D3
import Graphics.HSD3.Theme

------------------------------------------------------------------------------

type Chart a b = GraphT ThemeSt a b

data ThemeChart a b = ThemeChart (Theme a) (Chart a b)

instance Render ThemeChart where
    render dat (ThemeChart theme graph) =
        renderGraph (themeSt theme) dat (withPrelude theme graph)

------------------------------------------------------------------------------
