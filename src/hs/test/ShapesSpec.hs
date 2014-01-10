{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExtendedDefaultRules  #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ShapesSpec where

import Test.Hspec

import Soostone.Graphing.Base
import Soostone.Graphing.Shapes

import Utils

bar :: Anchor -> Graph Double ()
bar side = rect $ anchor side

-- Use sample for real tests
-- Use inProgress when working and it should fail

spec :: Spec
spec = describe "Shapes" $ do

    describe "Rectangles" $ do

        sample "a simple rectangle" 

            ([] :: [Int]) $

            rect $ return ()

        sample "a colored rectangle" 

            ([] :: [Int]) $

            rect $ attr "fill" "red"

    describe "Pad" $

        sample "a simple padding" 

            ([] :: [Int]) $

            pad 0.5 $ rect $ return ()

    describe "Split" $ do

        sample "a bar graph, anchored to the top"

            [1.0, 0.5, 0.25, 0.7, 0.9] $

            split Vertical $ bar Top

        sample "a horizontally oriented bar graph, anchored to the left" 

            [1.0, 0.5, 0.25, 0.7, 0.9] $

            split Horizontal (bar LeftSide)

        sample "a horizontally oriented bar graph, anchored to the tight" 

            [1.0, 0.5, 0.25, 0.7, 0.9] $

            split Horizontal (bar RightSide)

    describe "Grid" $

        sample "a simple grid"

            [[1.0, 0.5, 0.25, 0.7, 0.9], [0.5, 0.3, 0.25, 1.0, 0.4]] $

            grid $ pad 0.1 $ bar Top

