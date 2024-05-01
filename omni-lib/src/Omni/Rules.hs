module Omni.Rules (rules) where

import Control.Arrow (first)
import Control.Exception (IOException, catch)
import Control.Monad
import Control.Monad.IO.Class
import Data.Conduit.Process.Typed (ExitCode (..), proc, readProcess)
import Data.Foldable
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.Maybe
import Data.String.Conv (toS)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Lens.Micro
import Omni.Abs qualified as Parsed
import Omni.Config (Config, configBinariesDirectory, configExecutableOutputPath, configInputDirectories, configInputFiles, configOptLevel)
import Omni.Error (Report)
import Omni.Error qualified as Error
import Omni.Name qualified as Name
import Omni.Par qualified as Par
import Omni.Query
import Rock
import System.Directory
import System.FilePath (addExtension, joinPath, splitExtension, (<.>), (</>))
import Text.LLVM (Module (..))
import Text.LLVM qualified as LLVM
import Text.LLVM.PP qualified as LLVMPP
import Text.LLVM.Triple.Parse qualified as LLVMTriple
import Text.PrettyPrint qualified as PP

-- | Compilation rules, describing how to perform different tasks.
rules ::
  Config ->
  GenRules (Writer [Report] (Writer TaskKind Query)) Query
rules conf (Writer (Writer key)) = case key of
  SourceDirectories ->
    input $ success (conf ^. configInputDirectories)
  BinariesDirectory -> noError do
    let binDir = conf ^. configBinariesDirectory
    liftIO $ createDirectoryIfMissing True binDir
    return binDir
  Files ->
    input do
      srcDirs <- fetch SourceDirectories
      srcDirAbsolutes <- liftIO $ mapM makeAbsolute srcDirs
      filess <- forM srcDirAbsolutes \dir -> do
        allFileNames <- liftIO $ getDirectoryContents dir
        let allFiles = map (dir </>) allFileNames
            srcFiles =
              map (uncurry addExtension) $
                filter ((== ".omni") . snd) $
                  map splitExtension allFiles
        return srcFiles
      let files = concat filess
          inputFiles = conf ^. configInputFiles
      success $ HashSet.fromList $ files <> inputFiles
  FileText path ->
    input $
      liftIO do
        mbContents <- tryReadFile path
        case mbContents of
          Right contents -> success contents
          Left e -> failure $ Error.fileLoad path e
  ModuleFile name@(Name.Module nameText) ->
    nonInput do
      srcDirs <- fetch SourceDirectories
      files <- fetch Files
      let moduleFileName srcDir =
            srcDir
              </> joinPath (map Text.unpack $ Text.splitOn "." nameText)
              <.> "omni"
          potentialModulePaths = map moduleFileName srcDirs
          modulePaths = filter (`HashSet.member` files) potentialModulePaths
      case modulePaths of
        [] -> failure $ Error.moduleNotFound name
        [path] -> success $ Just path
        path : _ -> return (Just path, [Error.duplicatedModule name modulePaths])
  ParsedFile path ->
    nonInput do
      contents <- fetch $ FileText path
      case Par.pModule (Par.myLexer contents) of
        Left err -> return (Nothing, [Error.parse path err])
        Right parsedMod -> success $ Just parsedMod
  LLVMModule name ->
    nonInput do
      success
        LLVM.emptyModule
          { modTriple = LLVMTriple.parseTriple "x86_64-unknown-windows-gnu"
          }
  LLVMFiles ->
    noError do
      binDir <- fetch BinariesDirectory
      filePaths <- toList <$> fetch Files
      modules <- catMaybes <$> mapM (fetch . ParsedFile) filePaths
      forM modules \(Parsed.Module _ (Parsed.Ident name) _) -> do
        llvmModule <- fetch $ LLVMModule (Name.Module name)
        let llvmFileName = binDir </> toS name <.> "ll"
            llvmText =
              LLVMPP.withConfig
                (LLVMPP.Config 17)
                (PP.render . LLVMPP.llvmPP)
                llvmModule
        liftIO $ writeFile llvmFileName llvmText
        return llvmFileName
  Executable ->
    nonInput do
      binDir <- fetch BinariesDirectory
      llvmFiles <- fetch LLVMFiles
      if null llvmFiles
        then
          success Nothing
        else liftIO do
          let linkedOutputName = binDir </> "program" <.> "ll"
              optimizationLevel = conf ^. configOptLevel
              optimizedOutputName = binDir </> "program_opt" <.> "ll"
              executableName = conf ^. configExecutableOutputPath
              linkerArgs = ["-S", "-o", linkedOutputName] <> llvmFiles
          (linkerExitCode, _, linkerErr) <- readProcess $ proc "llvm-link" linkerArgs
          case linkerExitCode of
            ExitFailure _ -> failure $ Error.linking linkerArgs linkerErr
            ExitSuccess -> do
              (optExitCode, _, optErr) <-
                readProcess $
                  proc
                    "opt"
                    [show optimizationLevel, "-S", "-o", optimizedOutputName, linkedOutputName]
              case optExitCode of
                ExitFailure _ ->
                  failure $
                    Error.optimizing
                      optimizationLevel
                      linkedOutputName
                      optErr
                ExitSuccess -> do
                  (clangExitCode, _, clangErr) <-
                    readProcess $ proc "clang" ["-o", executableName, optimizedOutputName]
                  case clangExitCode of
                    ExitFailure _ -> failure $ Error.executableGeneration optimizedOutputName clangErr
                    ExitSuccess -> success (Just executableName)
 where
  -- \| For tasks whose results may change independently
  -- of their fetched dependencies.
  input :: (Functor m) => m (a, [Report]) -> m ((a, TaskKind), [Report])
  input = fmap (first (,Input))

  -- \| For tasks whose results only depend on fetched dependencies,
  -- that also don't need to return errors.
  noError :: (Functor m) => m a -> m ((a, TaskKind), [Report])
  noError = fmap ((,mempty) . (,NonInput))

  -- \| For tasks whose results only depend on fetched dependencies.
  nonInput :: (Functor m) => m (a, [Report]) -> m ((a, TaskKind), [Report])
  nonInput = fmap (first (,NonInput))

  -- \| Returns a successful value, without any errors.
  success :: (Applicative m) => a -> m (a, [Report])
  success = pure . (,mempty)

  -- \| Returns a failure, with an error report.
  failure :: (Applicative m, Monoid a) => Report -> m (a, [Report])
  failure = pure . (mempty,) . List.singleton

  -- \| Catches any exceptions related to file-reading.
  tryReadFile :: FilePath -> IO (Either IOException Text)
  tryReadFile path = (Right <$> Text.readFile path) `catch` (return . Left)