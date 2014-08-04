------------------------------------------------------------------------------

-- | Functions which wrap method calls in the D3 selection package.

------------------------------------------------------------------------------

{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Graphics.HSD3.D3.Selection where

import Language.Javascript.JMacro

import Graphics.HSD3.D3.Cursor
import Graphics.HSD3.D3.Graph
import Graphics.HSD3.D3.JMacro
import Graphics.HSD3.D3.Scope

------------------------------------------------------------------------------

-- | Creates a new SVG node of a specific type

append :: String -> GraphT s a JExpr
append el = withTarget [jmacroE|
    `(target)`.append(`(el)`)
|]

-- | Sets an attribute on the SVG __target__

attr :: ToCursor s a b => String -> b -> GraphT s a ()
attr key prop = do
    prop' <- toCursor prop
    insert [jmacro|
        `(target)`.attr(`(key)`, `(prop')`);
    |]

-- | Sets the style on the SVG __target__

style :: ToCursor s a b => String -> b -> GraphT s a ()
style key prop = do
    prop' <- toCursor prop
    insert [jmacro|
        `(target)`.style(`(key)`, `(prop')`);
    |]

-- | A D3 selectAll call - see D3 docs on Selections

selectAll :: String -> GraphT s a JExpr
selectAll n = withTarget [jmacroE|
    `(target)`.selectAll(`n`)
|]

-- | A D3 select call - see D3 docs on Selections

select :: String -> GraphT s a JExpr
select n = withTarget [jmacroE|
    `(target)`.select(`n`)
|]

-- | A D3 data call - see D3 docs on Selections.
--   TODO this will probably be broken into 2 `Graph`s eventually.

bind :: ToCursor s a b => b -> GraphT s a JExpr -> GraphT s d JExpr
bind r =
    fanout . with ent . scopeCursor
    where
        ent = do
            x <- toCursor r
            withTarget $ [jmacroE|
                `(target)`.data(`(x)`)
            |]

-- | A D3 enter call - see D3 docs on Selections.
--   TODO this will probably be broken into 2 `Graph`s eventually.

enter :: GraphT s a JExpr
enter =
    withTarget [jmacroE|
        `(target)`.enter()
    |]

call :: JExpr -> GraphT s a ()
call expr = insert [jmacro|
    `(target)`.call(`(expr)`);
|]

text :: ToCursor s a b => b -> GraphT s a ()
text prop = do
    p <- toCursor prop
    insert [jmacro|
        `(target)`.text(`(p)`);
    |]

linear :: GraphT s a JExpr
linear = withTarget [jmacroE| d3.scale.linear() |]

range :: ToCursor s a b => b -> GraphT s a ()
range b = do
    x <- toCursor b
    insert [jmacro|
       `(target)`.range(`(x)`);
    |]

domain :: ToCursor s a b => b -> GraphT s a ()
domain b = do
    x <- toCursor b
    insert [jmacro|
       `(target)`.domain(`(x)`);
    |]

tickFormat :: ToCursor s a b => b -> GraphT s a JExpr
tickFormat b = do
    x <- toCursor b
    withTarget [jmacroE|
       `(target)`.tickFormat(10, d3.format(`(x)`))
    |]

extent :: ToCursor s a b => b -> GraphT s a JExpr
extent gr = do
    ex <- scopeCursor . toCursor $ gr
    withTarget [jmacroE|
        d3.extent(__datum__, `(ex)`)
    |]

clamp :: ToCursor s a b => b -> GraphT s a JExpr
clamp gr = do
    ex <- scopeCursor . toCursor $ gr
    withTarget [jmacroE|
        [0, d3.max(__datum__, `(ex)`)]
    |]

nice :: GraphT s a ()
nice = insert [jmacro|
    `(target)`.nice();
|]

ticks :: ToCursor s a b => b -> GraphT s a JExpr
ticks c = do
    x <- toCursor c
    withTarget [jmacroE|
        `(target)`.ticks(`(x)`)
    |]

------------------------------------------------------------------------------
