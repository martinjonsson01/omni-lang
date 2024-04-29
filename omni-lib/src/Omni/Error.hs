module Omni.Error (Error (..)) where

import Control.Exception (IOException)
import Omni.Name qualified as Name

-- | Compiler errors.
data Error
  = FileLoad FilePath IOException
  | ModuleNotFound Name.Module
  | DuplicatedModule Name.Module [FilePath]
  deriving (Show)
