module Omni.Compiler (
  compile,
) where

import Omni.Config
import Omni.Driver
import Omni.Reporting (HydratedReport, hydrate)
import Omni.Query qualified as Query
import Rock (fetch)
import Prelude hiding (mod)

-- | Executes the full compiler on a given set of sources.
compile :: Config -> IO HydratedReport
compile conf = do
  (mbExePath, reports) <- runTask conf $ fetch Query.Executable
  case mbExePath of
    Just exePath -> putStrLn $ "Successfully compiled executable: " <> exePath
    Nothing -> pure ()
  fst <$> runTask conf (hydrate reports)
