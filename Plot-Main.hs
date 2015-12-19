module Main where

import Control.Monad (void)
import System.FilePath ((-<.>))
import System.Environment (getArgs)

import CFLPPlot

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("bench" : fileName : [])
      -> void $ readBench fileName (fileName -<.> "png")
    _ -> putStrLn "plot main usage"
