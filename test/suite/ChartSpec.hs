{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables  #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ChartSpec where

import Test.Hspec

import Graphics.HSD3.Chart
import Graphics.HSD3.Theme

import Utils

spec :: Spec
spec = describe "Chart" $ do

    describe "Bar Graph, with records" $ do

        sample "a simple graph, with a record"

            [Bar 1.0 "red", Bar 0.2 "green", Bar 0.3 "blue"]

            (ThemeChart def coloredBarGraph)

        sample "a more complicated record graph, with a record"

            (zipWith Bar
                (map ((+ 10) . (*10) . sin . (/5)) (take 15 [0..]))
                (concat $ repeat ["red", "red", "red", "green", "green", "green", "blue", "blue", "blue"]))

            (ThemeChart def coloredBarGraph)


    describe "Bar Graphs" $ do

        sample "a simple graph"

            [1.0, 0.2, 0.3]

            (ThemeChart def barGraph)

        sample "a large, but still simple bar graph"

            (take 50 . concat . repeat $
                [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8])

            (ThemeChart def barGraph)

        sample "a bar graph with arbitrary data size"

           (map ((+ 10) . (*10) . sin . (/5)) (take 75 [0..]))

            (ThemeChart def barGraph)


    describe "Stacked Bar Graphs" $ do

        sample "a trivial stacked bar graph"

            [
                take 50 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8]
            ]

            (ThemeChart def stackedBarGraph)

        sample "a simple stacked bar graph"

            [
                take 50 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8]
            ]

            (ThemeChart def stackedBarGraph)

        sample "a normal stacked bar graph"

            [
                take 50 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
                take 50 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8]
            ]

            (ThemeChart def stackedBarGraph)

        sample "a complex stacked bar graph"

            [
                take 50 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
                take 50 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
                take 50 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
                take 50 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
                take 50 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8]
            ]

            (ThemeChart def stackedBarGraph)

        sample "a complex stacked bar graph with different number of elements"

            [
                take 50 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
                take 40 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
                take 20 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
                take 2 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
                take 14 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8]
            ]

            (ThemeChart def stackedBarGraph)

        sample "a stacked bar graph with arbitrary data size"

            (replicate 10 (map ((+ 10) . (*10) . sin . (/5)) (take 75 [0..])))

            (ThemeChart def stackedBarGraph)

    describe "Grid layouts" $ do

        sample "a trivial grid layout"

            (replicate 5 [
                take 50 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
                take 50 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
                take 50 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
                take 50 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
                take 50 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8]
            ])

            (ThemeChart def gridBarGraph)

        sample "a non symmetrical grid layout"

            (fmap (uncurry take) . zip [5, 4, 4, 2, 5] . replicate 5 $ [
                take 50 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
                take 40 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
                take 30 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
                take 10 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
                take 50 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8]
            ])

            (ThemeChart def gridBarGraph)

        sample "a grid bar graph with arbitrary data size"

            (replicate 6 (replicate 6 (map ((+ 10) . (*10) . sin . (/5)) (take 75 [0..]))))

            (ThemeChart def gridBarGraph)




