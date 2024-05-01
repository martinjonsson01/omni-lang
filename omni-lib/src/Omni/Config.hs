{-# LANGUAGE TemplateHaskell #-}

module Omni.Config (
  Directory,
  Config (..),
  OptimizationLevel (..),
  config,
  configInputDirectories,
  configInputFiles,
  configBinariesDirectory,
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
  , _configInputFiles :: ![FilePath]
  , _configBinariesDirectory :: !Directory
  , _configExecutableOutputPath :: !FilePath
  , _configTraceFetch :: !Bool
  }
 deriving (Eq, Show)

instance Semigroup Config where
  a <> b =
    Config
      { _configOptLevel = _configOptLevel a <> _configOptLevel b
      , _configInputDirectories =
          _configInputDirectories a <> _configInputDirectories b
      , _configInputFiles = _configInputFiles a <> _configInputFiles b
      , _configBinariesDirectory = longestPath _configBinariesDirectory
      , _configExecutableOutputPath = longestPath _configExecutableOutputPath
      , _configTraceFetch = _configTraceFetch a || _configTraceFetch b
      }
   where
    longestPath selector =
      let locA = selector a
          locB = selector b
       in if length locA >= length locB then locA else locB

instance Monoid Config where
  mempty =
    Config
      { _configOptLevel = O0
      , _configInputDirectories = mempty
      , _configInputFiles = mempty
      , _configBinariesDirectory = "./bin"
      , _configExecutableOutputPath = "./program.exe"
      , _configTraceFetch = False
      }

-- | Which level of optimization to apply.
data OptimizationLevel = O0 | O1 | O2 | O3
 deriving (Eq)

-- | Higher optimization levels dominate.
instance Semigroup OptimizationLevel where
  O3 <> _ = O3
  _ <> O3 = O3
  O2 <> _ = O2
  _ <> O2 = O2
  O1 <> _ = O1
  _ <> O1 = O1
  O0 <> O0 = O0

instance Show OptimizationLevel where
  show = \case
    O0 -> "-O0"
    O1 -> "-O1"
    O2 -> "-O2"
    O3 -> "-O3"

makeClassy ''Config
