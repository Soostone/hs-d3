{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ShapesSpec where

import Test.Hspec

import Soostone.Graphing.Base
import Soostone.Graphing.Shapes
import Soostone.Graphing.Utils

import Utils

bar side h i = rect $ anchor side h

-- Use sample for real tests
-- Use inProgress when working and it should fail
spec = describe "Shapes" $ do

    describe "Rectangles" $ do

        sample "a simple rectangle" $

            rect $ return ()

        sample "a colored rectangle" $

            rect $ attr "fill" "red"

    describe "Bar Graphs" $ do

        sample "a bar graph, anchored to the top" $

            split Vertical (bar Top `mapI` [1.0, 0.5, 0.25, 0.7, 0.9] )

        sample "a horizontally oriented bar graph, anchored to the left" $

            split Horizontal (bar LeftSide `mapI` [1.0, 0.5, 0.25, 0.7, 0.9] )

        sample "a horizontally oriented bar graph, anchored to the tight" $

            split Horizontal (bar RightSide `mapI` [1.0, 0.5, 0.25, 0.7, 0.9] )

  
