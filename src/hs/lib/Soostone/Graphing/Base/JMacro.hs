------------------------------------------------------------------------------

-- | JMacro Utils

------------------------------------------------------------------------------

{-# LANGUAGE GADTs                 #-}

module Soostone.Graphing.Base.JMacro where

import Language.Javascript.JMacro

------------------------------------------------------------------------------

replace :: JMacro a => String -> JExpr -> a -> a
replace s x = 
    withHygiene (jfromGADT . composOp f . f . jtoGADT)
    where
        f :: JMGadt a -> JMGadt a
        f (JMGExpr (ValExpr (JVar (StrI z)))) | z == s = JMGExpr x
        f z = composOp f z

------------------------------------------------------------------------------