{-# OPTIONS_GHC -Wno-orphans #-}

module Omni.Monad (
  -- * The monad
  CompileM,

  -- * Run functions
  runCompileM,

  -- * Helpers
  report,
  withReport,
  failWith,
  fetchMaybe,
) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.Sequence qualified as Seq
import Data.Text.Lazy.Builder
import Katip
import Katip.Core
import Katip.Scribes.Handle
import Omni.Config
import Omni.Query (Query)
import Omni.Reporting
import Rock (MonadFetch (fetch), Task)
import System.IO (stderr, stdout)

-- | Top-level compilation monad.
newtype CompileM env a = CompileM
  { unwrapCompileM ::
      KatipContextT -- For logging.
        ( ReaderT -- For configuration.
            env
            ( MaybeT -- For fallible functions.
                ( WriterT
                    Reports -- For error/warning reporting.
                    (Task Query) -- For fetching.
                )
            )
        )
        a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , Alternative
    , MonadPlus
    , MonadIO
    , Katip
    , MonadReader env
    , MonadWriter Reports
    , MonadFail
    , MonadFetch Query
    )

instance (MonadFetch Query m) => MonadFetch Query (KatipContextT m)

-- | Reports and then short-circuits the current computation.
failWith :: Report -> CompileM env a
failWith r = report r >> mzero

-- | Include a given report in the output.
report :: Report -> CompileM env ()
report = tell . Seq.singleton

-- | Returns the given value, reporting the given error as a side-effect.
withReport :: a -> Report -> CompileM env a
withReport a r = tell (Seq.singleton r) >> return a

{- | Fetches the result of a query which might not return anything,
failing the computation if no result was returned.
-}
fetchMaybe :: Query (Maybe a) -> CompileM env a
fetchMaybe query = fetch query >>= maybe mzero return

-- | Executes the compilation monad.
runCompileM :: Config -> CompileM Config a -> Task Query (Maybe a, Reports)
runCompileM conf compileM = do
  stdoutScribe <-
    liftIO $
      mkHandleScribeWithFormatter
        logItemFormatter
        ColorIfTerminal
        stdout
        (permitItem ErrorS)
        V0
  stderrScribe <-
    liftIO $
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
    liftIO $
      initLogEnv "Omni" "production"
        >>= registerScribe "stdout" stdoutScribe defaultScribeSettings
        >>= registerScribe "stderr" stderrScribe defaultScribeSettings

  let m = unwrapCompileM compileM

  runWriterT $
    runMaybeT $
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
