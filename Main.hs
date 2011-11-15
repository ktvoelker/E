
module Main where

import System.Environment

import qualified Parser
import qualified LocalChecks
import qualified ImportResolution

import Message

compiler name text =
       Parser.phase name text
  ?>>? LocalChecks.phase
  ?>>? ImportResolution.phase

compile verbosity input output = do
  text <- readFile input
  x    <- runE verbosity $ compiler input text
  maybe (return ()) (writeFile output . show) x

main = do
  [verbosity, input, output] <- getArgs
  compile (read verbosity) input output

