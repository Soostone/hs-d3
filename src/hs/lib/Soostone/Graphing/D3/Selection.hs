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

attr :: ToCursor s a b => String -> a -> GraphT s b ()
attr key prop = do
    prop' <- toCursor prop
    insert [jmacro|
        `(target)`.attr(`(key)`, `(prop')`);
    |]

-- | Sets the style on the SVG __target__

style :: ToCursor s a b => String -> a -> GraphT s b ()
style key prop = do
    prop' <- toCursor prop
    insert [jmacro|
        `(target)`.style(`(key)`, `(prop')`);
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
    withTarget (Pre ent)
    where
        ent = [jmacroE| 
            `(target)`.data(`(cursor)`)
        |]

-- | A D3 enter call - see D3 docs on Selections. 
--   TODO this will probably be broken into 2 `Graph`s eventually.

enter :: GraphT s a () -> GraphT s [a] ()
enter =
    fanout . withTarget (Pre ent) . scopeCursor
    where
        ent = [jmacroE| 
            `(target)`.enter()
        |]

call :: JExpr -> GraphT s a ()
call expr = insert [jmacro|
    `(target)`.call(`(expr)`);
|]


text :: ToCursor s a b => a -> GraphT s b ()
text prop = do
    p <- toCursor prop
    insert [jmacro|
        `(target)`.text(`(p)`);
    |]

bindTicks :: GraphT s a () -> GraphT s a ()
bindTicks =
    withTarget (Pre ent)
    where
        ent = [jmacroE| 
            `(target)`.data(d3.scale.linear().ticks(10)).enter()
        |]

unitScale :: JExpr
unitScale = [jmacroE| d3.scale.linear().domain([0, 1]).range([1, 0]) |]

offsetScale :: JExpr
offsetScale = [jmacroE| d3.scale.linear().domain([-0.01, 0.99]).range([1, 0]) |]

invertScale :: JExpr
invertScale = [jmacroE| d3.scale.linear().domain([0, 1]).range([1, 0]).tickFormat(",.1f") |]

axis :: GraphT s a ()
axis =
    append "g" $ do
        attr "class" "axis"

        selectAll "line.y" $
            bindTicks $ do
                append "text" $ do
                    attr "x" 0
                    attr "y" offsetScale
                    text invertScale
                    
                append "line" $ do
                    attr "class" "y"
                    attr "x2" 1
                    attr "x1" 0
                    attr "y1" unitScale
                    attr "y2" unitScale
                    attr "stroke" "grey"
                    attr "stroke-width" "0.001"
       
------------------------------------------------------------------------------
