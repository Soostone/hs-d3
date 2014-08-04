{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables  #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ThemeSpec where

import Test.Hspec

import Graphics.HSD3.Chart
import Graphics.HSD3.Theme

import Utils

spec :: Spec
spec = describe "Themes" $ do

    describe "Bar Graphs" $ do

        sample "The default theme bar graph"

            (take 50 . concat . repeat $
                [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8])

            (ThemeChart def barGraph)

        sample "Colorful Banaani theme bar graph"

            (take 50 . concat . repeat $
                [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8])

            (ThemeChart banaaniTheme barGraph)

        sample "Rainbow theme bar graph"

            (take 50 . concat . repeat $
                [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8])

            (ThemeChart rainbowTheme barGraph)

    describe "Stacked Bar Graphs" $ do

        let dat = [
                    take 50 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
                    take 40 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
                    take 20 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
                    take 2 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
                    take 14 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8]
                ]

        sample "The default theme stacked bar graph"

            dat

            (ThemeChart def stackedBarGraph)

        sample "Colorful Banaani theme stacked bar graph"

            dat

            (ThemeChart banaaniTheme stackedBarGraph)

        sample "Rainbow theme stacked bar graph"

            dat

            (ThemeChart rainbowTheme stackedBarGraph)

    describe "Grid layouts" $ do

        let dat = fmap (uncurry take) . zip [5, 4, 4, 2, 5] . replicate 5 $ [
                    take 50 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
                    take 40 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
                    take 30 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
                    take 10 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
                    take 50 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8]
                ]

        sample "The default theme grid bar graph"

            dat

            (ThemeChart def gridBarGraph)

        sample "Colorful Banaani theme grid bar graph"

            dat

            (ThemeChart banaaniTheme gridBarGraph)

        sample "Rainbow theme grid bar graph"

            dat

            (ThemeChart rainbowTheme gridBarGraph)

