------------------------------------------------------------------------------

-- | Holds D3 level primitives.
--   TODO break me up as necessary!

------------------------------------------------------------------------------

{-# LANGUAGE ExtendedDefaultRules #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Graphics.HSD3.Theme
  ( module Graphics.HSD3.Theme.Base
  , module Graphics.HSD3.Theme.Prelude
  , rainbowTheme
  , banaaniTheme
  ) where


------------------------------------------------------------------------------
import Control.Monad
------------------------------------------------------------------------------
import Graphics.HSD3.D3
import Graphics.HSD3.Theme.Base
import Graphics.HSD3.Theme.Prelude
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Rainbow
rainbowTheme :: Theme a
rainbowTheme = def {

    themeSt = def {

        foreColor = Box $ cycl group [
                Hex "#096975",
                Hex "#7DB31B",
                Hex "#CFCF0C",
                Hex "#EA552D",
                Hex "#DB2556"
            ],

        backColor = Box $ Hex "LightBlue"
    }
}


------------------------------------------------------------------------------
-- | Colorful Banaani
--   http://www.colourlovers.com/palette/1606220/colorful_banaani
banaaniTheme :: Theme a
banaaniTheme = def {

    defs = void $ do
        void $ gradient Vertical "fourth" (RGB 223 214 191) (RGB 193 185 165)
        void $ gradient Vertical "fifth"  (RGB 150 186 175) (RGB 131 162 153)

        void $ gradient Vertical "first"  (RGB 150 186 175) (RGB 131 162 153)
        void $ gradient Vertical "second" (RGB 230 101 67)  (RGB 200 88 58)
        void $ gradient Vertical "third"  (RGB 196 47 52)   (RGB 171 41 46),

    themeSt = def {

        foreColor = Box $ cycl index [URL "first", URL "second", URL "third"],
        backColor = Box $ cycl index [URL "fourth"],

        strokeColor = Box $ cycl index [RGB 131 162 153, RGB 200 88 58, RGB 171 41 46],
        strokeWidth = Box (1 :: Integer)
    }
}
