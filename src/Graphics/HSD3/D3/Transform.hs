------------------------------------------------------------------------------

-- | A DSL for generating SVG transform strings.

------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}

module Graphics.HSD3.D3.Transform
  ( Transform()
  , rotate
  , scale
  , translate
  ) where

import Control.Monad.State
import Data.Monoid
import Control.Applicative
import Language.Javascript.JMacro

import Graphics.HSD3.D3.Cursor

------------------------------------------------------------------------------

-- | The principal monadic interface for generating transform strings.
--   `Transform` is a Writer monad (but we use state anyway ... we'll maybe
--   need it later).

--   TODO Should put some more thought into the defaults for `ToCursor` ...

newtype Transform a =
    Transform (State [JExpr] a)
    deriving (Functor, Applicative, Monad, MonadState [JExpr])

instance ToCursor s a (Transform ()) where
    toCursor = toCursor . toJExpr

runTransform :: Transform () -> [JExpr]
runTransform (Transform s) = execState s []

instance ToJExpr (Transform ()) where
    toJExpr x = [jmacroE| `(runTransform x)`.join(" ") |]

jstr :: String -> JExpr
jstr = ValExpr . JStr

-- | Scales by (x, y)

scale :: (ToJExpr a, ToJExpr b) => a -> b -> Transform ()
scale x y = app [
        jstr "scale(",
        toJExpr x,
        jstr ",",
        toJExpr y,
        jstr ")"
    ]

-- | Translates by (x, y)

translate :: (ToJExpr a, ToJExpr b) => a -> b -> Transform ()
translate x y = app [
        jstr "translate(",
        toJExpr x,
        jstr ",",
        toJExpr y,
        jstr ")"
    ]

-- | Rotates by x

rotate :: ToJExpr a => a -> Transform ()
rotate x = app [jstr "rotate(", toJExpr x, jstr")" ]

app :: [JExpr] -> Transform ()
app = modify . flip mappend

------------------------------------------------------------------------------
