{-# LANGUAGE ExtendedDefaultRules  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module TransformSpec where

import Test.Hspec

import Soostone.Graphing.D3

import Utils

testRect :: Transform () -> GraphT () a ()
testRect tr = select "body" $ append "svg" $ do
    attr "width" (640 :: Integer)
    attr "height" (480 :: Integer)

    append "rect" $ do
        attr "x" (0 :: Integer)
        attr "y" (0 :: Integer)
        attr "width" (100 :: Integer)
        attr "height" (100 :: Integer)
        attr "fill" "black"
        attr "transform" tr

empty :: [Integer]
empty = []

spec :: Spec
spec = describe "Shapes" $ do

    describe "Scales" $ do

        sample "a simple scale" 

            empty $

            testRect $ scale 0.5 0.5

        sample "a complex scale" 

            empty $

            testRect $ do
                scale 0.5 0.1
                scale 2.0 10

    describe "Translates" $ do

        sample "a simple translate" 

            empty $

            testRect $ translate 300 300

        sample "a complex translate" 

            empty $

            testRect $ do
                translate 300 300
                translate (-300) (-300)

    describe "Compund transforms" $

        sample "a compound transform" 

            empty $

            testRect $ do
                scale 0.5 0.5
                translate 300 300
                rotate 20


  
