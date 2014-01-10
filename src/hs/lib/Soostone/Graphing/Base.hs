------------------------------------------------------------------------------

-- | Core library components.  Most anything that touches raw JMacro should
--   go in this module for now.

------------------------------------------------------------------------------

{-# LANGUAGE QuasiQuotes           #-}

module Soostone.Graphing.Base(
    module Soostone.Graphing.Base.Cursor,
    module Soostone.Graphing.Base.D3,
    module Soostone.Graphing.Base.Graph,
    module Soostone.Graphing.Base.Prelude,
    module Soostone.Graphing.Base.Transform,
    onload
) where

import Language.Javascript.JMacro

import Soostone.Graphing.Base.Cursor
import Soostone.Graphing.Base.D3
import Soostone.Graphing.Base.Graph
import Soostone.Graphing.Base.Prelude
import Soostone.Graphing.Base.Transform

------------------------------------------------------------------------------

-- | Convenience method for attaching behavior to the browser window's
--   onload method.

onload :: JStat -> JStat
onload js = [jmacro|
    window.onload = function () { `(js)`; };
|]

------------------------------------------------------------------------------
