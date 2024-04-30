module Omni.Rules (rules) where

import Control.Arrow (first)
import Control.Exception (IOException, catch)
import Control.Monad
import Control.Monad.IO.Class
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Lens.Micro
import Omni.Config (Config, configInputDirectories)
import Omni.Error (Error)
import Omni.Error qualified as Error
import Omni.Name qualified as Name
import Omni.Par qualified as Par
import Omni.Query
import Rock
import System.Directory
import System.FilePath (joinPath, splitExtension, (<.>), (</>))
import qualified Text.LLVM as LLVM

-- | Compilation rules, describing how to perform different tasks.
rules ::
  Config ->
  GenRules (Writer [Error] (Writer TaskKind Query)) Query
rules conf (Writer (Writer key)) = case key of
  SourceDirectories ->
    input $ success (conf ^. configInputDirectories)
  Files ->
    input do
      srcDirs <- fetch SourceDirectories
      filess <- forM srcDirs \dir -> do
        allFiles <- liftIO $ getDirectoryContents dir
        let srcFiles = filter (== ".omni") $ map (snd . splitExtension) allFiles
        return srcFiles
      let files = concat filess
      success $ HashSet.fromList files
  FileText path ->
    input $
      liftIO do
        mbContents <- tryReadFile path
        case mbContents of
          Right contents -> success contents
          Left e -> failure $ Error.FileLoad path e
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
        [] -> failure $ Error.ModuleNotFound name
        [path] -> success $ Just path
        path : _ -> return (Just path, [Error.DuplicatedModule name modulePaths])
  ParsedFile path ->
    nonInput do
      contents <- fetch $ FileText path
      case Par.pModule (Par.myLexer contents) of
        Left err -> return (Nothing, [Error.Parse path err])
        Right parsedMod -> success $ Just parsedMod
  LLVMModule name -> 
    nonInput do 
      success LLVM.emptyModule
 where
  -- \| For tasks whose results may change independently
  -- of their fetched dependencies.
  input :: (Functor m) => m (a, [Error]) -> m ((a, TaskKind), [Error])
  input = fmap (first (,Input))

  -- \| For tasks whose results only depend on fetched dependencies,
  -- that also don't need to return errors.
  noError :: (Functor m) => m a -> m ((a, TaskKind), [Error])
  noError = fmap ((,mempty) . (,NonInput))

  -- \| For tasks whose results only depend on fetched dependencies.
  nonInput :: (Functor m) => m (a, [Error]) -> m ((a, TaskKind), [Error])
  nonInput = fmap (first (,NonInput))

  -- \| Returns a successful value, without any errors.
  success :: (Applicative m) => a -> m (a, [Error])
  success = pure . (,mempty)

  -- \| Returns a failure, with an error report.
  failure :: (Applicative m, Monoid a) => Error -> m (a, [Error])
  failure = pure . (mempty,) . List.singleton

  -- \| Catches any exceptions related to file-reading.
  tryReadFile :: FilePath -> IO (Either IOException Text)
  tryReadFile path = (Right <$> Text.readFile path) `catch` (return . Left)