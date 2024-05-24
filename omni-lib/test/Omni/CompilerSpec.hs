module Omni.CompilerSpec (spec) where

import Control.Exception.Base
import Control.Monad
import Omni.Compiler
import Omni.Config
import Omni.Reporting qualified as Reporting
import System.Directory
import System.FilePath
import Test.Syd
import qualified Data.Text as Text

spec :: Spec
spec =
  describe "compile" do
    goodFiles <- liftIO $ getGoldenFiles Good
    forM_ goodFiles \file -> do
      it ("compiles " <> file <> " without error") do
        goldenTextFile (addExtension file ".golden") do
          errors <-
            compile (createConfig file)
              `catch` \(e :: SomeException) -> do pPrint e; error (ppShow e)
          return $ Reporting.prettyRenderPlainText errors

    badFiles <- liftIO $ getGoldenFiles Bad
    forM_ badFiles \file -> do
      it ("compiling " <> file <> " produces error(s)") do
        goldenTextFile (addExtension file ".golden") do
          errors <-
            compile (createConfig file)
              `catch` \(e :: SomeException) -> do pPrint e; error (ppShow e)
          return $ Reporting.prettyRenderPlainText errors
 where
  createConfig file =
    let srcFile = addExtension file ".omni"
        binLoc = "test_resources" </> "bin" </> takeBaseName file
     in mempty
          { _configInputFiles = [srcFile]
          , _configMainModule = Text.pack $ takeBaseName file
          , _configBinariesDirectory = binLoc
          , _configExecutableOutputPath = binLoc </> "program" <.> "exe"
          }

-- | Good golden examples should succeed compilation, bad ones should fail.
data GoldenVariant = Good | Bad

-- | Gets a list of all golden example file paths (excluding file extension).
getGoldenFiles :: GoldenVariant -> IO [FilePath]
getGoldenFiles variant = do
  let subDir = case variant of Good -> "omni_good"; Bad -> "omni_bad"
      dir = "test_resources" </> subDir
  allFileNames <- liftIO $ getDirectoryContents dir
  let allFiles = map (dir </>) allFileNames
      fileNames =
        map fst $
          filter ((== ".omni") . snd) $
            map splitExtension allFiles
  return fileNames