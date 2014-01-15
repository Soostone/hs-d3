------------------------------------------------------------------------------

-- | Functions which wrap method calls in the D3 selection package.

------------------------------------------------------------------------------

{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE ExtendedDefaultRules #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Soostone.Graphing.D3.Selection where

import Language.Javascript.JMacro

import Soostone.Graphing.D3.Cursor
import Soostone.Graphing.D3.Graph

------------------------------------------------------------------------------

-- | Creates a new SVG node of a specific type

append :: String -> GraphT s a () -> GraphT s a ()
append el = withTarget [jmacroE| 
    `(target)`.append(`(el)`) 
|]

-- | Sets an attribute on the SVG __target__

attr :: ToCursor s a => String -> a -> GraphT s b ()
attr key prop = do
    prop' <- toCursor prop
    insert [jmacro|
        `(target)`.attr(`(key)`, `(prop')`);
    |]

-- | A D3 selectAll call - see D3 docs on Selections

selectAll :: String -> GraphT s a () -> GraphT s a ()
selectAll n = withTarget [jmacroE| 
    `(target)`.selectAll(`n`)
|]

-- | A D3 select call - see D3 docs on Selections

select :: String -> GraphT s a () -> GraphT s a ()
select n = withTarget [jmacroE| 
    `(target)`.select(`n`)
|]

-- | A D3 data call - see D3 docs on Selections. 
--   TODO this will probably be broken into 2 `Graph`s eventually.

bindData :: GraphT s a () -> GraphT s a ()
bindData =
    withTarget (Prefix ent)
    where
        ent = [jmacroE| 
            `(target)`.data(`(cursor)`)
        |]

-- | A D3 enter call - see D3 docs on Selections. 
--   TODO this will probably be broken into 2 `Graph`s eventually.

enter :: GraphT s a () -> GraphT s [a] ()
enter =
    fanout . withTarget (Prefix ent) . scopeCursor
    where
        ent = [jmacroE| 
            `(target)`.enter()
        |]

------------------------------------------------------------------------------
