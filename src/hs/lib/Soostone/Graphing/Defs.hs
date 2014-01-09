


dropShadow :: Graph ()
dropShadow =
    withDefs (defNode "filter") $ do
        defAttr "id" "drop-shadow"
        defAttr "height" "130%"
        withDefs (defNode "feGaussianBlur") $ do
            defAttr "in" "SourceAlpha"
            defAttr "stdDeviation" (5 :: Int)
            defAttr "result" "blur"
        withDefs (defNode "feOffset") $ do
            defAttr "in" "blur"
            defAttr "dx" (5 :: Int)
            defAttr "dyoffsetBlur" (5 :: Int)
            defAttr "result" "blur"
        withDefs (defNode "feMerge") $ do
            withDefs (defNode "feMergeNode") $ defAttr "in" "offsetBlur"
            withDefs (defNode "feMergeNode") $ defAttr "in" "SourceGraphic"

withDefs :: JExpr -> Graph () -> Graph ()
withDefs st gr = do
    inner <- extract gr
    insert [jmacro|
        (function(x) {
            var !__defs__ = x;
            `(inner)`;
        })(`(st)`);
    |]


defAttr :: ToJExpr a => String -> a -> Graph ()
defAttr target prop = 
    insert [jmacro|
        __defs__.attr(`(target)`, `(prop)`);
    |]


style :: ToJExpr a => String -> a -> Graph ()
style target prop = 
    insert [jmacro|
        __target__.style(`(target)`, `(prop)`);
    |]



defNode :: String -> JExpr
defNode el = [jmacroE| __defs__.append(`(el)`) |]