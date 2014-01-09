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
    
phantomjs :: String -> Graph a -> IO ExitCode
phantomjs title x = do

    writeGraph "test.html" x

    (_, _, Just e, p) <-
        createProcess (proc "phantomjs" ["src/js/test/render.js", title]) { std_err = CreatePipe }
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


sample :: String -> Graph a -> Spec
sample title source = do
    gold <- getGold
    case gold of
        Just gold ->
            it ("Should render" ++ title) $ do
                phantomjs "test.png" source
                graph <- B.readFile "test.png"
                assertEqual "test" graph gold
                
        Nothing -> it ("Writing gold record: " ++ title ++ ";  please rerun suite") $
            inProgress' title source pendingWith


    where
        getGold = case (show (abs $ hash title) ++ ".png") `lookup` golds of
            Nothing -> return Nothing
            Just x  -> return $ Just x


inProgress title source = it ("In Progress: " ++ title) $ inProgress' title source assertFailure 

inProgress' title source asert = do
    phantomjs ("src/obj/test/" ++ show (abs $ hash title) ++ ".png") source
    asert ("Generated record for \"" ++ title ++ "\"")
