{-# LANGUAGE NoImplicitPrelude #-}

module Types (
  App (..),
  Options (..),
  Input (..),
  Output (..),
  OptimizationLevel(..),
) where

import RIO
import RIO.Process

-- | Command line arguments.
data Options = Options
  { optionsInput :: !Input
  , optionsOutput :: !Output
  , optionsVerbose :: !Bool
  , optionsOpt :: !OptimizationLevel
  }

-- | Compiler input modes.
data Input = StdIn | InputFile !FilePath

-- | Compiler output modes.
data Output = StdOut | OutputFile !FilePath

-- | Which level of optimization to apply.
data OptimizationLevel = O0 | O1 | O2 | O3

-- | Application config.
data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x{appLogFunc = y})
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x{appProcessContext = y})
