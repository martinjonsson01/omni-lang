module Omni.Monad (CompileM, runCompileM) where

import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import Data.Text.Lazy.Builder
import Katip
import Katip.Core
import Katip.Scribes.Handle
import Omni.Query (Query)
import Omni.Config
import Rock
import System.IO (stderr, stdout)

-- | Top-level compilation monad.
newtype CompileM a = CompileM
  { unwrapCompileM :: KatipContextT (ReaderT Config IO) a
  }
  deriving (Functor, Applicative, Monad, MonadIO, Katip, MonadReader Config)

-- | Executes the compilation monad.
runCompileM :: Config -> CompileM a -> Task Query a
runCompileM conf compileM = liftIO do
  stdoutScribe <-
    mkHandleScribeWithFormatter
      logItemFormatter
      ColorIfTerminal
      stdout
      (permitItem ErrorS)
      V0
  stderrScribe <-
    mkHandleScribeWithFormatter
      logItemFormatter
      ColorIfTerminal
      stderr
      ( permitAND
          (permitItem NoticeS)
          (\item -> return $ _itemSeverity item < WarningS)
      )
      V0
  logEnv <-
    initLogEnv "Omni" "production"
      >>= registerScribe "stdout" stdoutScribe defaultScribeSettings
      >>= registerScribe "stderr" stderrScribe defaultScribeSettings

  let m = unwrapCompileM compileM

  runReaderT
    (runKatipContextT logEnv (mempty :: LogContexts) mempty m)
    conf

-- | Custom terminal log-item formatter.
logItemFormatter :: (LogItem a) => ItemFormatter a
logItemFormatter withColor verb Item{..} =
  brackets (mconcat $ map fromText $ intercalateNs _itemNamespace)
    <> brackets (fromText (renderSeverity' _itemSeverity))
    <> mconcat ks
    <> maybe mempty (brackets . fromString . locationToString) _itemLoc
    <> fromText " "
    <> unLogStr _itemMessage
 where
  ks = map brackets $ getKeys verb _itemPayload
  renderSeverity' severity =
    colorBySeverity withColor severity (renderSeverity severity)
