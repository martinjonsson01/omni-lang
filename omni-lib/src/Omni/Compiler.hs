module Omni.Compiler (
  compile,
) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Conduit.Process.Typed (proc, runProcess_)
import Data.Foldable
import Data.Maybe (catMaybes)
import Data.String.Conv (toS)
import Lens.Micro
import Omni.Abs qualified as Parsed
import Omni.Config
import Omni.Driver
import Omni.Name qualified as Name
import Omni.Query
import Omni.Query qualified as Query
import Rock (fetch)
import Rock qualified
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((<.>), (</>))
import Text.LLVM.PP qualified as LLVMPP
import Text.PrettyPrint qualified as PP
import Prelude hiding (mod)

-- | Executes the full compiler on a given set of sources.
compile :: Config -> IO ()
compile conf = do
  (_, errors) <- runTask conf (compileTask conf)
  print errors

-- | The task of compiling sources to executable binaries.
compileTask :: Config -> Rock.Task Query ()
compileTask conf = do
  let binDirLoc = conf ^. configBinariesDirectoryLocation
      binDir = binDirLoc </> "bin"
  liftIO $ createDirectoryIfMissing True binDir

  filePaths <- toList <$> fetch Query.Files
  modules <- catMaybes <$> mapM (fetch . Query.ParsedFile) filePaths

  if null modules
    then
      return ()
    else do
      llvmFiles <-
        forM modules \(Parsed.Module _ (Parsed.Ident name) _) -> do
          llvmModule <- fetch $ Query.LLVMModule (Name.Module name)
          let llvmFileName = binDir </> toS name <.> "ll"
              llvmText =
                LLVMPP.withConfig
                  (LLVMPP.Config 17)
                  (PP.render . LLVMPP.llvmPP)
                  llvmModule
          liftIO $ writeFile llvmFileName llvmText
          return llvmFileName

      liftIO do
        let linkedOutputName = binDir </> "program" <.> "ll"
            optimizationLevel = conf ^. configOptLevel
            optimizedOutputName = binDir </> "program_opt" <.> "ll"
            executableName = conf ^. configExecutableOutputPath
        runProcess_ $ proc "llvm-link" $ ["-S", "-o", linkedOutputName] <> llvmFiles
        runProcess_ $
          proc
            "opt"
            [show optimizationLevel, "-S", "-o", optimizedOutputName, linkedOutputName]
        runProcess_ $ proc "clang" ["-o", executableName, optimizedOutputName]