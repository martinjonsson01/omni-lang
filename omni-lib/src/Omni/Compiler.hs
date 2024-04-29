

module Omni.Compiler (
  compile
) where

import Omni.Query
import Rock qualified 
import Omni.Config
import Omni.Driver

-- | Executes the full compiler on a given set of sources.
compile :: Config -> IO ()
compile conf = do 
  (_, errors) <- runTask conf compileTask
  print errors

-- | The task of compiling sources to executable binaries.
compileTask :: Rock.Task Query ()
compileTask = do
  undefined