{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Utils where

import Control.Monad
import Data.Hashable
import System.Directory
import Test.Hspec


import Control.Concurrent
import Control.Exception          as E
import Data.FileEmbed
import Data.Maybe
import Language.Javascript.JMacro
import Network.HTTP
import Network.URI
import System.Exit
import System.IO
import System.IO.Unsafe
import System.Process
import Test.HUnit

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BU


import Graphics.HSD3.D3
import Graphics.HSD3.Repl

nodejs :: String -> IO String
nodejs js = do

    (Just std_in', Just std_out', Just std_err', p) <-
        createProcess (proc "node" []) { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

    hPutStrLn std_in' $ js ++ ";process.exit(0);\n"

    z <- waitForProcess p
    status <- hGetContents std_out'
    errs <- hGetContents std_err'

    case z of
        ExitFailure n -> return $ "FATAL: error code " ++ show n ++ "\n" ++ errs
        ExitSuccess   -> return status

phantomjs :: (ToJExpr a, Render b) => EmbedMode -> String -> a -> b a () -> IO ()

--phantomjs d3 title a b = do
--    handle <- openFile (title ++ ".html") WriteMode
--    writeGraph d3 handle a b
--    hClose handle

--    (_, _, Just e, p) <-
--        createProcess (proc "phantomjs" ["lib/js/render.js", title ++ ".html", title ++ ".png"]) { std_err = CreatePipe }
--    z <- waitForProcess p
--    hClose e
--    return z


phantomjs d3 title a b = do
    handle <- openFile (title ++ ".html") WriteMode
    writeGraph d3 handle a b
    hClose handle
    phantomjs' title

phantomjs' :: forall a. HStream [a] => [a] -> IO ()
phantomjs' x = do
    let uri = fromMaybe undefined $ parseURI "http://127.0.0.1:1337"
        args = [ mkHeader HdrContentLength (show$ length x)
             , mkHeader HdrContentType "application/x-www-form-urlencoded" ]

    result <- E.try $ simpleHTTP (Request uri POST args x) >>= getResponseBody
    case result of
        Right _ -> return ()
        Left (_ :: SomeException) -> do
            evalServer
            phantomjs' x

evalServer :: IO ()
evalServer = do

    ch <- newEmptyMVar

    _ <- forkOS $ do

        (Nothing, Just std_out', Just std_err', p) <-
            createProcess (proc "phantomjs" ["lib/js/render.js"]) {
                std_out = CreatePipe,
                std_err = CreatePipe,
                close_fds = True
            }

        hSetBuffering std_out' NoBuffering
        status <- hGetContents std_out'
        length (takeWhile (/= '\n') status) `seq` putMVar ch ()
        errs <- hGetContents std_err'
        putStrLn errs
        z <- length status `seq` waitForProcess p
        case z of
            ExitFailure _ -> error "Don't go into an infinite loop!"
            ExitSuccess -> do
                putStrLn "Closed"
                return ()

    takeMVar ch

jsServer :: String
jsServer = BU.unpack $(embedFile "lib/js/render.js")



golds :: [(String, B.ByteString)]
golds = unsafePerformIO $ do
    files' <- getDirectoryContents "test/testcase-output"
    files <- foldM filt [] files'
    bins <- sequence (fmap (B.readFile . ("test/testcase-output/"++)) files)
    return (zip files bins)

    where
        filt acc file = do
            exists <- doesFileExist ("test/testcase-output/" ++ file)
            return $ if exists then file : acc else acc


sample :: (ToJExpr a, Render b) => String -> a -> b a () -> Spec
sample title dat source = do
    gold' <- getGold "png"
    goldHtml <- getGold "html"
    case (gold', goldHtml) of
        (Just gold, Just html) -> do
            it ("Should generate javascript for " ++ title) $ do
                _ <- phantomjs cdnD3 "test" dat source
                graph' <- B.readFile "test.html"
                assertEqual "test" graph' html
            it ("Should render " ++ title) $ do
                graph' <- B.readFile "test.png"
                assertEqual "test" graph' gold

        _ -> it ("Writing gold record: " ++ title ++ ";  please rerun suite") $
            inProgress' title dat source pendingWith


    where
        getGold x = case (show (abs $ hash title) ++ "." ++ x) `lookup` golds of
            Nothing -> return Nothing
            Just xx  -> return $ Just xx


inProgress :: (ToJExpr a, Render b) => String -> a -> b a () -> Spec
inProgress title dat source =
    it ("In Progress: " ++ title)
        $ inProgress' title dat source assertFailure

cdnD3 :: EmbedMode
cdnD3 = Path "http://cdnjs.cloudflare.com/ajax/libs/d3/3.3.13/d3.min.js"

inProgress' :: (ToJExpr a, Render b) => String -> a -> b a () -> (String -> IO ()) -> IO ()
inProgress' title dat source asert = do
    _ <- phantomjs cdnD3 ("test/testcase-output/" ++ show (abs $ hash title)) dat source
    asert ("Generated record for \"" ++ title ++ "\"")
