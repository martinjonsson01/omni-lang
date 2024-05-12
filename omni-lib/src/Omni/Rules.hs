module Omni.Rules (rules) where

import Control.Arrow (first)
import Control.Exception (IOException, catch)
import Data.HashSet qualified as HashSet
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Omni.Codegen.External
import Omni.Config (Config, configBinariesDirectory, configInputDirectories, configInputFiles)
import Omni.Imports
import Omni.Name qualified as Name
import Omni.Par qualified as Par
import Omni.Query
import Omni.Reporting qualified as Reporting
import Omni.TypeCheck.L00AST qualified as L0
import Omni.TypeCheck.L00ParseAST (convertParsed)
import Omni.TypeCheck.L01Rename (renameIdents)
import Rock
import Text.LLVM qualified as LLVM

-- | Compilation rules, describing how to perform different tasks.
rules ::
  Config ->
  GenRules (Writer Reports (Writer TaskKind Query)) Query
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
          Left e -> failure $ Reporting.fileLoad path e
  ModuleFile name@(Name.ModuleName nameText) ->
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
        [] -> failure $ Reporting.moduleNotFound name
        [path] -> success $ Just path
        path : _ -> return (Just path, Seq.singleton $ Reporting.duplicatedModule name modulePaths)
  ParsedFile path ->
    nonInput do
      contents <- fetch $ FileText path
      case Par.pModule (Par.myLexer contents) of
        Left err -> return (Nothing, Seq.singleton $ Reporting.parse path err)
        Right parsedMod -> runCompileM conf (convertParsed path parsedMod)
  FileDefinitions path ->
    nonInput $
      first (fromMaybe []) <$> runCompileM conf do
        (L0.Module _ _ defs) <- fetchMaybe $ ParsedFile path
        return defs
  RenamedFile path ->
    nonInput $ runCompileM conf do
      fetchMaybe (ParsedFile path) >>= renameIdents
  LLVMModule _ ->
    nonInput $
      (success . snd . LLVM.runLLVM) do
        LLVM.define LLVM.emptyFunAttrs (LLVM.iT 32) "main" () do
          LLVM.label "entry"
          LLVM.comment "test comment"
          LLVM.ret (LLVM.iT 32 LLVM.-: (0 :: Int))
  LLVMFiles ->
    nonInput $ first concat <$> runCompileM conf generateLLVMModules
  Executable ->
    nonInput $ runCompileM conf generateExecutable
 where
  -- \| For tasks whose results may change independently
  -- of their fetched dependencies.
  input :: (Functor m) => m (a, Reports) -> m ((a, TaskKind), Reports)
  input = fmap (first (,Input))

  -- \| For tasks whose results only depend on fetched dependencies,
  -- that also don't need to return errors.
  noError :: (Functor m) => m a -> m ((a, TaskKind), Reports)
  noError = fmap ((,mempty) . (,NonInput))

  -- \| For tasks whose results only depend on fetched dependencies.
  nonInput :: (Functor m) => m (a, Reports) -> m ((a, TaskKind), Reports)
  nonInput = fmap (first (,NonInput))

  -- \| Returns a successful value, without any errors.
  success :: (Applicative m) => a -> m (a, Reports)
  success = pure . (,mempty)

  -- \| Returns a failure, with an error report.
  failure :: (Applicative m, Monoid a) => Report -> m (a, Reports)
  failure = pure . (mempty,) . Seq.singleton

  -- \| Catches any exceptions related to file-reading.
  tryReadFile :: FilePath -> IO (Either IOException Text)
  tryReadFile path = (Right <$> Text.readFile path) `catch` (return . Left)