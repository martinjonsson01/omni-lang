{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Import
import Options.Applicative.Simple
import qualified Paths_omni_cli
import RIO.Process
import Run

main :: IO ()
main = do
  (options, ()) <-
    simpleOptions
      $(simpleVersion Paths_omni_cli.version)
      "Omni Compiler"
      "A compiler for the Omni programming language."
      ( Options
          <$> inputParser
          <*> outputParser
          <*> verbosityParser
          <*> optLevelParser
      )
      empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app =
          App
            { appLogFunc = lf
            , appProcessContext = pc
            , appOptions = options
            }
     in runRIO app run

inputParser :: Parser Input
inputParser =
  argument (InputFile <$> str)
    $ metavar "SOURCE_FILE"
    <> help
      "Source file to compile. \
      \ If not given, input is read from standard input."
    <> value StdIn

outputParser :: Parser Output
outputParser =
  option (OutputFile <$> str)
    $ short 'o'
    <> long "output-file"
    <> metavar "FILE"
    <> help
      "Output file for the compilation result. \
      \ If not given, output is written to standard output."
    <> value StdOut

verbosityParser :: Parser Bool
verbosityParser =
  switch
    ( long "verbose"
        <> short 'v'
        <> help "Verbose output?"
    )

optLevelParser :: Parser OptimizationLevel
optLevelParser =
  option parseOptLevel
    $ short 'O'
    <> metavar "LEVEL"
    <> help
      "Optimization level. \
      \ If not given, no optimization is applied (i.e. -O0)."
    <> value O0
 where
  parseOptLevel :: ReadM OptimizationLevel
  parseOptLevel = do
    level <- auto
    case level :: Int of
      0 -> return O0
      1 -> return O1
      2 -> return O2
      3 -> return O3
      _ -> readerError "Valid optimization levels are 0 - 3"
