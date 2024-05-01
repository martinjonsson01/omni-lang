module Omni.Error (
  -- * Types
  Report,
  HydratedReport,

  -- * Constructors
  fileLoad,
  moduleNotFound,
  duplicatedModule,
  parse,
  linking,
  optimizing,
  executableGeneration,

  -- * Pretty-printing
  hydrate,
  prettyRender,
) where

import Control.Exception (IOException)
import Data.ByteString.Lazy qualified as Lazy
import Data.List (intersperse)
import Data.String.Conv (toS)
import Data.Text (Text)
import Data.Text qualified as Text
import Omni.Config (OptimizationLevel)
import Omni.Name qualified as Name
import Omni.Query (Query)
import Rock (Task)
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass (Pretty (..))
import Text.Show.Pretty (ppShow)

-- | Compiler output.
data Report = Warn Warning | Err Error

-- Warnings -----------------------------------------------

-- | Non-critical information about the compilation.
data Warning

-- Errors -------------------------------------------------

-- | Critical failures in the compilation.
data Error
  = -- File errors
    FileLoad FilePath IOException
  | ModuleNotFound Name.Module
  | DuplicatedModule Name.Module [FilePath]
  | -- Parse errors
    Parse FilePath String
  | -- LLVM errors
    Linking [String] Lazy.ByteString
  | Optimizing OptimizationLevel FilePath Lazy.ByteString
  | ExecutableGeneration FilePath Lazy.ByteString
  deriving (Show)

fileLoad :: FilePath -> IOException -> Report
fileLoad path = Err . FileLoad path

moduleNotFound :: Name.Module -> Report
moduleNotFound = Err . ModuleNotFound

duplicatedModule :: Name.Module -> [FilePath] -> Report
duplicatedModule name = Err . DuplicatedModule name

parse :: FilePath -> String -> Report
parse path = Err . Parse path

linking :: [String] -> Lazy.ByteString -> Report
linking args = Err . Linking args

optimizing :: OptimizationLevel -> FilePath -> Lazy.ByteString -> Report
optimizing level path = Err . Optimizing level path

executableGeneration :: FilePath -> Lazy.ByteString -> Report
executableGeneration path = Err . ExecutableGeneration path

-- Pretty-printing ----------------------------------------

-- | A report which has been supplemented with additional context.
newtype HydratedReport = Hydrated {getHydratedDocs :: [Doc]}

-- | Supplements the given errors with additional context.
hydrate :: [Report] -> Task Query HydratedReport
hydrate reports = Hydrated <$> mapM hydrateReport reports
 where
  hydrateReport :: Report -> Task Query Doc
  hydrateReport = \case
    Warn warn -> hydrateWarning warn
    Err err -> hydrateError err

  hydrateWarning :: Warning -> Task Query Doc
  hydrateWarning _ = return empty

  hydrateError :: Error -> Task Query Doc
  hydrateError = \case
    Linking args output ->
      return $
        headerAndBody
          "linking error:"
          (hsep $ map text args)
          (multilineOutput output)
    Optimizing level inputPath output ->
      return $
        headerAndBody
          "optimizing error:"
          (hsep [text $ show level, text inputPath])
          (multilineOutput output)
    ExecutableGeneration inputPath output ->
      return $
        headerAndBody
          "executable generation error:"
          (text inputPath)
          (multilineOutput output)
    generic -> return $ text $ ppShow generic
   where
    headerAndBody :: Doc -> Doc -> Doc -> Doc
    headerAndBody header extras body = header $$ nest 2 (extras $+$ nest 2 body)

    multilineOutput :: Lazy.ByteString -> Doc
    multilineOutput output =
      vcat $
        map (text . toS) $
          Text.splitOn "\r\n" (toS output)

-- | Renders the list of hydrated errors in a human-friendly manner.
prettyRender :: HydratedReport -> Text
prettyRender = Text.pack . render . pPrint

instance Pretty HydratedReport where
  pPrint = vcat . intersperse divider . getHydratedDocs
   where
    blank :: Doc
    blank = text ""

    divider :: Doc
    divider = blank $+$ hcat (replicate 20 $ char '-') $+$ blank
