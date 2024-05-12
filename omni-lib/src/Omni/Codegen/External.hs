module Omni.Codegen.External (generateLLVMModules, generateExecutable) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString.Lazy qualified as Lazy
import Data.Conduit.Process.Typed
import Data.Foldable
import Data.Maybe
import Data.String.Conv
import Lens.Micro
import Omni.Config
import Omni.Monad
import Omni.Name qualified as Name
import Omni.Query qualified as Query
import Omni.Reporting
import Omni.Reporting qualified as Error
import Omni.TypeCheck.L01RenamedAST qualified as L1
import Rock (fetch)
import System.FilePath
import Text.LLVM.PP qualified as LLVMPP

-- | Generates LLVM IR modules for each source module.
generateLLVMModules :: CompileM env [FilePath]
generateLLVMModules = do
  binDir <- fetch Query.BinariesDirectory
  filePaths <- toList <$> fetch Query.Files
  modules <- catMaybes <$> mapM (fetch . Query.RenamedFile) filePaths
  forM modules \(L1.Module _ (Name.ModuleName name) _) -> do
    llvmModule <- fetch $ Query.LLVMModule (Name.ModuleName name)
    let llvmFileName = binDir </> toS name <.> "ll"
        llvmText =
          LLVMPP.withConfig
            (LLVMPP.Config 17)
            (show . LLVMPP.llvmPP)
            llvmModule
    liftIO $ writeFile llvmFileName llvmText
    return llvmFileName

-- | Calls out into external tools to compile the generated LLVM IR into an executable.
generateExecutable :: (HasConfig env) => CompileM env FilePath
generateExecutable = do
  binDir <- fetch Query.BinariesDirectory
  llvmFiles <- fetch Query.LLVMFiles

  when (null llvmFiles) $ failWith $ noLLVMIRModules binDir

  env <- ask
  let linkedOutputName = binDir </> "program" <.> "ll"
      optimizationLevel = env ^. configOptLevel
      optimizedOutputName = binDir </> "program_opt" <.> "ll"
      executableName = env ^. configExecutableOutputPath
      linkerArgs = ["-S", "-o", linkedOutputName] <> llvmFiles
      makeLinkReport = Error.linking linkerArgs . snd
      linkProcess = proc "llvm-link" linkerArgs
  liftProcess linkProcess makeLinkReport

  let makeOptReport = Error.optimizing optimizationLevel linkedOutputName . snd
      optProcess =
        proc
          "opt"
          [show optimizationLevel, "-S", "-o", optimizedOutputName, linkedOutputName]
  liftProcess optProcess makeOptReport

  let makeExeReport = Error.executableGeneration optimizedOutputName . snd
      exeProcess = proc "clang" ["-o", executableName, optimizedOutputName]
  liftProcess exeProcess makeExeReport
  return executableName
 where
  liftProcess ::
    ProcessConfig stdin stdoutIgnored stderrIgnored ->
    ((Lazy.ByteString, Lazy.ByteString) -> Report) ->
    CompileM env ()
  liftProcess process makeFailureReport = do
    (exitCode, out, err) <- liftIO $ readProcess process
    case exitCode of
      ExitSuccess -> return ()
      ExitFailure _ -> failWith $ makeFailureReport (out, err)