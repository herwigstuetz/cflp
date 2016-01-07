module Main where

import Control.Monad (void)
import System.FilePath ((-<.>), (</>), takeExtension)
import System.Directory (getDirectoryContents)
import System.Environment (getArgs)

import           Text.Parsec.Prim

import CFLP
import CFLPPlot

readCFLPFile :: FilePath -> IO (Maybe CFLP)
readCFLPFile fileName = do
  cflp <- readFile fileName
  return $ case parse cflpFile "cflp" cflp of
    Left  _    -> Nothing
    Right cflp -> Just cflp

plotCFLPFile :: FilePath -> IO ()
plotCFLPFile fileName = do
  cflp <- readCFLPFile fileName
  case cflp of
    Just cflp -> plotCFLP cflp (fileName -<.> "png")
    Nothing -> putStrLn ("Could not read " ++ fileName)


readSolvedCFLPFile :: FilePath -> IO (Maybe CFLP)
readSolvedCFLPFile fileName = do
  cflp <- readFile fileName
  return $ case parse solvedCflpFile "cflp" cflp of
    Left  _    -> Nothing
    Right cflp -> Just cflp

plotSolvedCFLPFile :: FilePath -> IO ()
plotSolvedCFLPFile fileName = do
  cflp <- readSolvedCFLPFile fileName
  case cflp of
    Just cflp -> plotCFLP cflp (fileName -<.> "png")
    Nothing -> putStrLn ("Could not read " ++ fileName)


main :: IO ()
main = do
  args <- getArgs
  case args of
    ("bench" : fileName : [])
      -> void $ readBench fileName (fileName -<.> "png")
    ("gen" : dirName : [])
      -> do
        plotCFLPFile (dirName </> "problem.cflp")
        plotSolvedCFLPFile (dirName </> "exact.sol")
        plotSolvedCFLPFile (dirName </> "approx.sol")
    _ -> putStrLn "plot main usage"
