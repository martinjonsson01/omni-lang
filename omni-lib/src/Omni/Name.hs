module Omni.Name (Module(..)) where 
import Data.Text
import Data.String
import Data.Hashable
import Prettyprinter

newtype Module = Module Text 
 deriving (Eq, Ord, Show, IsString, Hashable)

instance Pretty Module where 
  pretty (Module t) = pretty t