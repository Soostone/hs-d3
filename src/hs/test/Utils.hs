{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Utils where

import System.IO
import System.Process
import System.Exit
import Test.HUnit
import Test.Hspec

import qualified Data.ByteString as B

import Control.Monad
import Data.Hashable
import System.IO.Unsafe
import System.Directory
import Language.Javascript.JMacro


import Soostone.Graphing.Base
import Soostone.Graphing.Repl

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
    
phantomjs :: ToJExpr a => String -> String -> a -> Graph a b -> IO ExitCode
phantomjs d3 title dat x = do
    writeGraph d3 (title ++ ".html") x dat

    (_, _, Just e, p) <-
        createProcess (proc "phantomjs" ["src/js/test/render.js", title ++ ".html", title ++ ".png"]) { std_err = CreatePipe }
    z <- waitForProcess p
    hClose e
    return z


golds :: [(String, B.ByteString)]
golds = unsafePerformIO $ do
    files' <- getDirectoryContents "src/obj/test"
    files <- foldM filt [] files'
    bins <- sequence (fmap (B.readFile . ("src/obj/test/"++)) files)
    return (zip files bins)

    where
        filt acc file = do
            exists <- doesFileExist ("src/obj/test/" ++ file)
            return $ if exists then file : acc else acc


sample :: ToJExpr a => String -> a -> Graph a b -> Spec
sample title dat source = do
    gold' <- getGold "png"
    goldHtml <- getGold "html"
    case (gold', goldHtml) of
        (Just gold, Just html) -> do
            it ("Should generate javascript for " ++ title) $ do
                _ <- phantomjs "lib/js/d3.v2.min.js" "test" dat source
                graph' <- B.readFile "test.html"
                assertEqual "test" (B.drop 19 graph') (B.drop 28 html)
            it ("Should render " ++ title) $ do
                graph' <- B.readFile "test.png"
                assertEqual "test" graph' gold
                
        _ -> it ("Writing gold record: " ++ title ++ ";  please rerun suite") $
            inProgress' title dat source pendingWith


    where
        getGold x = case (show (abs $ hash title) ++ "." ++ x) `lookup` golds of
            Nothing -> return Nothing
            Just xx  -> return $ Just xx


inProgress :: ToJExpr a => String -> a -> Graph a b -> Spec
inProgress title dat source =
	it ("In Progress: " ++ title)
		$ inProgress' title dat source assertFailure

inProgress' :: ToJExpr a => String -> a -> Graph a b -> (String -> IO ()) -> IO ()
inProgress' title dat source asert = do
    _ <- phantomjs "../../../lib/js/d3.v2.min.js" ("src/obj/test/" ++ show (abs $ hash title)) dat source
    asert ("Generated record for \"" ++ title ++ "\"")
