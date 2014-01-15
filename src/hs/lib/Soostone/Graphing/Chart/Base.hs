------------------------------------------------------------------------------

-- | Holds D3 level primitives.
--   TODO break me up as necessary!

------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Soostone.Graphing.Chart.Base where

import Soostone.Graphing.D3
import Soostone.Graphing.Theme

------------------------------------------------------------------------------

type Chart a b = GraphT ThemeSt a b

data ThemeChart a b = ThemeChart (Theme a) (Chart a b)

instance Render ThemeChart where
    render dat (ThemeChart theme graph) =
        renderGraph (themeSt theme) dat (withPrelude theme graph)

------------------------------------------------------------------------------
