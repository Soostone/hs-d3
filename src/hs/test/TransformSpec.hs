{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExtendedDefaultRules  #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TransformSpec where

import Test.Hspec

import Soostone.Graphing.Base
import Soostone.Graphing.Shapes

import Utils

spec :: Spec
spec = describe "Shapes" $ do

    describe "Scales" $ do

        sample "a simple scale" 

            ([] :: [Int]) $

            rect $ attr "transform" $ scale 0.5 0.5

        sample "a complex scale" 

            ([] :: [Int]) $

            rect $ attr "transform" $ do
                scale 0.5 0.1
                scale 2.0 10

    describe "Translates" $ do

        sample "a simple translate" 

            ([] :: [Int]) $

            rect $ attr "transform" $ translate 0.5 0.5

        sample "a complex translate" 

            ([] :: [Int]) $

            rect $ attr "transform" $ do
                translate 0.5 0.1
                translate (-0.5) (-0.1)

    describe "Compund transforms" $

        sample "a compound transform" 

            ([] :: [Int]) $

            rect $ attr "transform" $ do
                scale 0.5 0.5
                translate 0.5 0.5
                rotate 20


  
