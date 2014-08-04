------------------------------------------------------------------------------

-- | JMacro Utils

------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Graphics.HSD3.D3.JMacro where

import Data.Monoid
import Language.Javascript.JMacro

------------------------------------------------------------------------------

replace :: JMacro a => String -> JExpr -> a -> a
replace s x =
    withHygiene (jfromGADT . f . jtoGADT)
    where
        f :: JMGadt a -> JMGadt a
        f (JMGExpr (ValExpr (JVar (StrI z)))) | z == s = JMGExpr x
        f z = composOp f z

instance JMacro () where
    jtoGADT () = jtoGADT mempty
    jfromGADT _ = ()

------------------------------------------------------------------------------
