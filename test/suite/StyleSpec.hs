{-# LANGUAGE ExtendedDefaultRules #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module StyleSpec where

import Control.Monad
import Test.Hspec

import Graphics.HSD3.D3

import Utils

testRect :: GraphT () a () -> GraphT () a ()
testRect tr = void $ select "body" `with` append "svg" `with` do

    _ <- append "defs" `with` do
        _ <- dropShadow "test-shadow"
        _ <- gradient Vertical "test-gradient" (RGB 200 100 0) (RGB 0 100 200)
        gradient Horizontal "test-gradient2" (RGB 200 100 0) (RGB 0 100 200)

    attr "width" (640 :: Double)
    attr "height" (480 :: Double)

    append "rect" `with` do
        attr "x" (100 :: Double)
        attr "y" (100 :: Double)
        attr "width" (100 :: Double)
        attr "height" (100 :: Double)
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
