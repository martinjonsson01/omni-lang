module Omni.Driver (runTask) where

import Omni.Imports

import Control.Concurrent
import Data.Dependent.HashMap (DHashMap, DSum ((:=>)))
import Data.Dependent.HashMap qualified as DHashMap
import Data.IORef
import Data.Text qualified as Text
import Omni.Config (Config, configTraceFetch)

import Omni.Query
import Omni.Rules qualified as Rules
import Rock (
  GenRules,
  Rules,
  Task,
  TaskKind,
  Writer (..),
  memoiseWithCycleDetection,
  traceFetch,
  writer,
 )
import Rock qualified

{- | Executes the given compiler task in a sequential non-incremental manner.
Meant to be called when compiling as a one-shot, otherwise
parallel + incremental compilation would be more efficient.
-}
runTask :: Config -> Task Query a -> IO (a, Reports)
runTask conf task = do
  startedVar <- newIORef mempty
  errorsVar <- newIORef (mempty :: DHashMap Query (Const Reports))
  printVar <- newMVar 0
  threadDepsVar <- newIORef mempty

  let writeErrors :: Writer TaskKind Query a -> Reports -> Task Query ()
      writeErrors (Writer q) errs =
        unless (null errs) $
          liftIO $
            atomicModifyIORef' errorsVar $
              (,()) . DHashMap.insert q (Const errs)

      -- \| The TaskKind is included for the incremental compilation,
      -- but can be ignored for this usecase.
      ignoreTaskKind :: Query a -> TaskKind -> Task Query ()
      ignoreTaskKind _ _ =
        pure ()

      -- Logs all fetch-requests if enabled, for debugging purposes.
      traceFetch_ ::
        GenRules (Writer TaskKind Query) Query ->
        GenRules (Writer TaskKind Query) Query
      traceFetch_
        | conf ^. configTraceFetch =
            traceFetch
              ( \(Writer key) -> liftIO $ modifyMVar_ printVar \n -> do
                  putText $ fold (replicate n "| ") <> "fetching " <> tshow key
                  return $ n + 1
              )
              ( \_ _ -> liftIO $ modifyMVar_ printVar \n -> do
                  putText $ fold (replicate (n - 1) "| ") <> "*"
                  return $ n - 1
              )
        | otherwise = id

      rules :: Rules Query
      rules =
        memoiseWithCycleDetection startedVar threadDepsVar $
          writer ignoreTaskKind $
            traceFetch_ $
              writer writeErrors $
                Rules.rules conf

  Rock.runTask rules do
    result <- task
    errorsMap <- liftIO $ readIORef errorsVar
    let errors =
          flip foldMap (DHashMap.toList errorsMap) \(_ :=> Const errs) ->
            errs
    pure (result, errors)

putText :: Text -> IO ()
putText = putStrLn . Text.unpack

tshow :: (Show a) => a -> Text
tshow = Text.pack . show
