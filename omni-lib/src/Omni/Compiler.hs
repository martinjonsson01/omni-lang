module Omni.Compiler (
  compile,
) where

import Omni.Config
import Omni.Driver
import Omni.Query qualified as Query
import Omni.Reporting (HydratedReport, hydrate)
import Rock (fetch)
import Prelude hiding (mod)

-- | Executes the full compiler on a given set of sources.
compile :: Config -> IO HydratedReport
compile conf = do
  (_, reports) <- runTask conf $ fetch Query.Executable
  fst <$> runTask conf (hydrate reports)
