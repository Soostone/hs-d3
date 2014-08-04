{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables  #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ShapesSpec where

import Control.Monad
import Test.Hspec

import Graphics.HSD3.Chart
import Graphics.HSD3.D3
import Graphics.HSD3.Theme

import Utils

bar :: Anchor -> Chart Double ()
bar side = void $ rect `with` anchor side

-- Use sample for real tests
-- Use inProgress when working and it should fail

spec :: Spec
spec = describe "Shapes" $ do

    describe "Rectangles" $ do

        sample "a simple rectangle"

            ([] :: [Int]) $

            ThemeChart def $ void rect

        sample "a colored rectangle"

            ([] :: [Int]) $

            ThemeChart def $ void $ rect `with` attr "fill" "red"

    describe "Pad" $

        sample "a simple padding"

            ([] :: [Int]) $

            ThemeChart def $ void $ pad 0.5 rect

    describe "Split" $ do

        sample "a bar graph, anchored to the top"

            [1.0, 0.5, 0.25, 0.7, 0.9] $

            ThemeChart def $ void $ splitVertical $ bar Top

        sample "a horizontally oriented bar graph, anchored to the left"

            [1.0, 0.5, 0.25, 0.7, 0.9] $

            ThemeChart def $ void $ splitHorizontal (bar LeftSide)

        sample "a horizontally oriented bar graph, anchored to the tight"

            [1.0, 0.5, 0.25, 0.7, 0.9] $

            ThemeChart def $ void $ splitHorizontal (bar RightSide)

    describe "Grid" $

        sample "a simple grid"

            [[1.0, 0.5, 0.25, 0.7, 0.9], [0.5, 0.3, 0.25, 1.0, 0.4]] $

            ThemeChart def $ void $ grid $ pad 0.1 $ bar Top

