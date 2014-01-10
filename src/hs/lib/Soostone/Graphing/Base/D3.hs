------------------------------------------------------------------------------

-- | D3 wrapper.  This module exports direct wrappers around D3 calls.

------------------------------------------------------------------------------

{-# LANGUAGE QuasiQuotes           #-}

module Soostone.Graphing.Base.D3 where

import Language.Javascript.JMacro

import Soostone.Graphing.Base.Cursor
import Soostone.Graphing.Base.Graph

------------------------------------------------------------------------------

-- | Creates a new SVG node of a specific type

append :: String -> Graph a () -> Graph a ()
append el = withTarget [jmacroE| 
    `(target)`.append(`(el)`) 
|]

-- | Sets an attribute on the SVG __target__

attr :: ToCursor a => String -> a -> Graph b ()
attr key prop = do
    prop' <- toCursor prop
    insert [jmacro|
        `(target)`.attr(`(key)`, `(prop')`);
    |]

-- | A D3 selectAll call - see D3 docs on Selections

selectAll :: String -> Graph a () -> Graph a ()
selectAll n = withTarget [jmacroE| 
    `(target)`.selectAll(`n`)
|]

-- | A D3 data and enter call - see D3 docs on Selections. 
--   TODO this will probably be broken into 2 `Graph`s eventually.

enter :: Graph a () -> Graph [a] ()
enter =
    fanout . withTarget (Prefix ent) . scopeCursor
    where
        ent = [jmacroE| 
            `(target)`.data(`(cursor)`).enter()
        |]

------------------------------------------------------------------------------
