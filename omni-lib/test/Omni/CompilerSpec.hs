module Omni.CompilerSpec (spec) where

import Control.Monad
import Omni.Compiler
import Omni.Config
import Omni.Reporting qualified as Error
import System.Directory
import System.FilePath
import Test.Syd

spec :: Spec
spec =
  describe "compile" do
    files <- liftIO $ getGoldenFiles Good
    forM_ files \file -> do
      it ("compiles " <> file <> " without error") do
        goldenTextFile (addExtension file ".golden") do
          let srcFile = addExtension file ".omni"
              binLoc = "test_resources" </> "bin" </> takeBaseName file
              conf =
                mempty
                  { _configInputFiles = [srcFile]
                  , _configBinariesDirectory = binLoc
                  }
          errors <- compile conf
          return $ Error.prettyRenderAnsiText errors

-- | Good golden examples should succeed compilation, bad ones should fail.
data GoldenVariant = Good | Bad

-- | Gets a list of all golden example file paths (excluding file extension).
getGoldenFiles :: GoldenVariant -> IO [FilePath]
getGoldenFiles variant = do
  let subDir = case variant of Good -> "good"; Bad -> "bad"
      dir = "test_resources" </> subDir
  allFileNames <- liftIO $ getDirectoryContents dir
  let allFiles = map (dir </>) allFileNames
      fileNames =
        map fst $
          filter ((== ".omni") . snd) $
            map splitExtension allFiles
  return fileNames