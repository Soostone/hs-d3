------------------------------------------------------------------------------

-- | REPL tools for debugging/interacting/developing `Graph`s

------------------------------------------------------------------------------

{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Graphics.HSD3.Repl
  ( graph
  , writeGraph
  , EmbedMode( .. )
  ) where


------------------------------------------------------------------------------
import           Control.Concurrent
import qualified Data.ByteString.Char8         as B
import           Data.FileEmbed
import           Language.Javascript.JMacro
import           System.Exit
import           System.IO
import           System.IO.Temp
import           System.Process
import           Text.InterpolatedString.Perl6
------------------------------------------------------------------------------
import Graphics.HSD3.D3
------------------------------------------------------------------------------


------------------------------------------------------------------------------
data EmbedMode = Inline | Path String


------------------------------------------------------------------------------
-- | Displays a graph in Chrome on OSX.
graph :: (ToJExpr a, Render b) => a -> b a () -> IO ()
graph a b = withSystemTempFile "temp.html" $ \file handle -> do
    writeGraph Inline handle a b
    hFlush handle
    exit <- system $ "open " ++ file
    threadDelay 1000000
    case exit of
        ExitFailure _ -> putStrLn "Failed to open browser"
        ExitSuccess -> return ()


------------------------------------------------------------------------------
-- | Writes a `Chart` to an HTML file.
writeGraph
    :: (ToJExpr a, Render b)
    => EmbedMode
    -> Handle
    -> a
    -> b a ()
    -> IO ()
writeGraph d3 file a b =
    hPutStr file $ wrapHTML d3Text $ render a b
  where
    d3Text = case d3 of
        Inline -> "<script>" ++ d3lib ++ "</script>"
        Path x -> "<script src=\"" ++ x ++ "\"></script>"


------------------------------------------------------------------------------
-- | Appends the basic HTML necessary to bootstrap a graph into the browser.
wrapHTML :: String -> String -> String
wrapHTML d3 js = [qq|

    $d3

    <style>
        body \{
            padding: 0;
            margin: 0;
        \}

        .domain \{
            fill: none;
        \}

        .tick line \{
            stroke-width: 0.001;
            stroke: grey;
        \}

        text \{
            font-size: 0.02;
            fill: grey;
            font-family: Helvetica;

        \}
    </style>

    <script> $js </script>

|]


------------------------------------------------------------------------------
d3lib :: String
d3lib = B.unpack $(embedFile "lib/js/d3.v2.min.js")

