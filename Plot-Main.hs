module Main where

import Control.Monad (void)
import System.FilePath ((-<.>), (</>))
import System.Environment (getArgs)


import           Text.Parsec.Prim

import CFLP
import CFLPPlot

readCFLPFile :: FilePath -> IO (Maybe CFLP)
readCFLPFile fileName = do
  cflp <- readFile fileName
  return $ case parse cflpFile "cflp" cflp of
    Left _ -> Nothing
    Right cflp -> Just cflp

plotCFLPFile :: FilePath -> IO ()
plotCFLPFile fileName = do
  Just cflp <- readCFLPFile fileName
  plotCFLP cflp (fileName -<.> "png")

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("bench" : fileName : [])
      -> void $ readBench fileName (fileName -<.> "png")
    ("gen" : dirName : [])
      -> do
        plotCFLPFile (dirName </> "problem.cflp")
        plotCFLPFile (dirName </> "exact.cflp")
        plotCFLPFile (dirName </> "approx.cflp")
    _ -> putStrLn "plot main usage"
