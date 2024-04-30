{-# LANGUAGE TemplateHaskell #-}

module Omni.Config (
  Directory,
  Config (..),
  OptimizationLevel (..),
  config,
  configInputDirectories,
  configBinariesDirectoryLocation,
  configExecutableOutputPath,
  configOptLevel,
  configTraceFetch,
) where

import Lens.Micro.TH (makeClassy)

type Directory = FilePath

-- | Compiler-wide configuration.
data Config = Config
  { _configOptLevel :: !OptimizationLevel
  , _configInputDirectories :: ![Directory]
  , _configBinariesDirectoryLocation :: !Directory
  , _configExecutableOutputPath :: !FilePath
  , _configTraceFetch :: !Bool
  }

-- | Which level of optimization to apply.
data OptimizationLevel = O0 | O1 | O2 | O3

instance Show OptimizationLevel where
  show = \case
    O0 -> "-O0"
    O1 -> "-O1"
    O2 -> "-O2"
    O3 -> "-O3"

makeClassy ''Config
