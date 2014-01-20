{-# LANGUAGE ExtendedDefaultRules  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module StyleSpec where

import Test.Hspec

import Soostone.Graphing.D3

import Utils

testRect :: GraphT () a () -> GraphT () a ()
testRect tr = select "body" $ append "svg" $ do

    append "defs" $ do
        dropShadow "test-shadow"
        gradient Vertical "test-gradient" (RGB 200 100 0) (RGB 0 100 200)
        gradient Horizontal "test-gradient2" (RGB 200 100 0) (RGB 0 100 200)
 
    attr "width" 640
    attr "height" 480

    append "rect" $ do
        attr "x" 100
        attr "y" 100
        attr "width" 100
        attr "height" 100
        attr "fill" "lightblue"
        tr

empty :: [Integer]
empty = []

spec :: Spec
spec = describe "Styles" $ do

    describe "Drop Shadows" $

        sample "a simple drop shadow" 

            empty $

            testRect $ style "filter" $ URL "test-shadow"

    describe "Gradients" $ do

        sample "a simple vertical gradient" 

            empty $

            testRect $ style "fill" $ URL "test-gradient"

        sample "a simple horizontal gradient" 

            empty $

            testRect $ style "fill" $ URL "test-gradient2"            