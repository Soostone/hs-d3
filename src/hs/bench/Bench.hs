{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExtendedDefaultRules      #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Criterion.Main

import Control.Concurrent
import Control.Exception          as E
import Control.Monad
import Data.FileEmbed
import Data.Maybe
import Data.String.Utils
import Language.Javascript.JMacro
import Network.HTTP
import Network.URI
import System.Directory
import System.Exit
import System.IO
import System.IO.Unsafe
import System.Process

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BU

import Soostone.Graphing.Chart
import Soostone.Graphing.D3
import Soostone.Graphing.Repl
import Soostone.Graphing.Theme

write title a b = do
    handle <- openFile (title ++ ".html") WriteMode
    writeGraph Inline handle a b
    hClose handle

phantomjs x = do
    let uri = fromMaybe undefined $ parseURI "http://127.0.0.1:1337"
        args = [ mkHeader HdrContentLength (show$ length x)
             , mkHeader HdrContentType "application/x-www-form-urlencoded" ]

    result <- E.try $ simpleHTTP (Request uri POST args x) >>= getResponseBody
    case result of
        Right z -> return ()
        Left (err :: SomeException) -> do
            evalServer
            phantomjs x

evalServer :: IO ()
evalServer = do

    ch <- newEmptyMVar

    forkOS $ do

        (Nothing, Just std_out', Just std_err', p) <-
            createProcess (proc "phantomjs" ["src/js/test/render.js"]) {
                std_out = CreatePipe,
                std_err = CreatePipe,
                close_fds = True
            }

        hSetBuffering std_out' NoBuffering
        status <- hGetContents std_out'
        length (takeWhile (/= '\n') status) `seq` putMVar ch ()
        errors <- hGetContents std_err'
        putStrLn errors
        z <- length status `seq` waitForProcess p
        case z of
            ExitFailure _ -> error "Don't go into an infinite loop!"
            ExitSuccess -> do
                putStrLn "Closed"
                return ()

    takeMVar ch

jsServer :: String
jsServer = BU.unpack $(embedFile "src/js/test/render.js")

data Case = forall a b. ToJExpr a => Case String a (ThemeChart a b)

dat = fmap (uncurry take) . zip [5, 4, 4, 2, 5] . replicate 5 $ [
        take 50 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
        take 40 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
        take 30 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
        take 10 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
        take 50 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8]
    ]



charts :: [Case]
charts = [
    Case "Bar Graph"
        (take 50 . concat . repeat $
                [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8])

        (ThemeChart def barGraph),

    Case "Stacked Bar Graph"
        [
                take 50 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
                take 50 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
                take 50 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
                take 50 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
                take 50 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8]
            ]

            (ThemeChart def stackedBarGraph),

    Case "Grid Bar Graph"
        (fmap (uncurry take) . zip [5, 4, 4, 2, 5] . replicate 5 $ [
                take 50 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
                take 40 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
                take 30 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
                take 10 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8],
                take 50 . concat . repeat $ [1.0, 0.5, 0.25, 0.7, 0.9, 0.76, 0.2, 0.3, 0.1, 1.0, 0.6, 0.4, 0.8]
            ])

            (ThemeChart def gridBarGraph),

    Case "Rainbow Theme"

            dat

            (ThemeChart def gridBarGraph),

    Case "Baanani Theme"

            dat

            (ThemeChart banaaniTheme gridBarGraph)

    ]

golds :: [(String, String)]
golds = take 5 $ unsafePerformIO $ do
    files' <- getDirectoryContents "src/obj/test"
    files <- foldM filt [] files'
    bins <- sequence (fmap (fmap BU.unpack . B.readFile . ("src/obj/test/"++)) files)
    return (zip files bins)

    where
        filt acc file = do
            exists <- doesFileExist ("src/obj/test/" ++ file)
            return $ if (exists && endswith "html" file) then file : acc else acc

main :: IO ()
main = do
    evalServer
    defaultMain [
        bgroup "Generating" $ fmap (\(Case n x y) -> bench n $ nf (uncurry renderGraph) (y, x)) charts

        --bgroup "Rendering" $ fmap (\(x, y) -> bench ("TestCase " ++ x) $ nfIO (phantomjs ("src/obj/test/" ++ x))) golds
        ]
