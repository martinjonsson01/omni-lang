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
  prettyRenderPlainText,
  prettyRenderAnsiText,
) where

import Control.Exception (IOException)
import Data.ByteString.Lazy qualified as Lazy
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Omni.Config (OptimizationLevel)
import Omni.Name qualified as Name
import Omni.Query (Query)
import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), bold, color, colorDull, italicized)
import Prettyprinter.Render.Terminal qualified as PPTerminal
import Prettyprinter.Render.Text qualified as PPText
import Rock (Task)
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

-- | Extra data associated with a report document.
data ReportAnnotation
  = Header
  | Context
  | Content

-- | Report document describing e.g. compiler errors or warnings.
type ReportDoc = Doc ReportAnnotation

-- | A report which has been supplemented with additional context.
newtype HydratedReport = Hydrated {getHydratedDocs :: [ReportDoc]}

-- | Supplements the given errors with additional context.
hydrate :: [Report] -> Task Query HydratedReport
hydrate reports = Hydrated <$> mapM hydrateReport reports
 where
  hydrateReport :: Report -> Task Query ReportDoc
  hydrateReport = \case
    Warn warn -> hydrateWarning warn
    Err err -> hydrateError err

  hydrateWarning :: Warning -> Task Query ReportDoc
  hydrateWarning _ = return mempty

  hydrateError :: Error -> Task Query ReportDoc
  hydrateError = \case
    Linking args output ->
      return $
        headerAndBody
          "linking error:"
          (hsep $ map pretty args)
          (processOutput Content output)
    Optimizing level inputPath output ->
      return $
        headerAndBody
          "optimizing error:"
          (hsep [pretty $ show level, pretty inputPath])
          (processOutput Content output)
    ExecutableGeneration inputPath output ->
      return $
        headerAndBody
          "executable generation error:"
          (pretty inputPath)
          (processOutput Content output)
    generic -> return $ pretty $ ppShow generic
   where
    headerAndBody :: ReportDoc -> ReportDoc -> ReportDoc -> ReportDoc
    headerAndBody header extras body =
      vsep
        [ annotate Header header
        , indent
            2
            ( vsep
                [ annotate Context extras
                , indent 2 (annotate Content body)
                ]
            )
        ]

    processOutput :: ReportAnnotation -> Lazy.ByteString -> ReportDoc
    processOutput ann =
      vsep
        . map (annotate ann . pretty)
        . Text.lines
        . TextEncoding.decodeUtf8Lenient
        . Lazy.toStrict

{- | Renders the list of hydrated errors in a human-friendly manner,
in a plaintext format without text styling.
-}
prettyRenderPlainText :: HydratedReport -> Text
prettyRenderPlainText =
  PPText.renderStrict
    . layoutPretty defaultLayoutOptions
    . prettyReport

{- | Renders the list of hydrated errors in a human-friendly manner,
in a rich format with text styling (ANSI colors).
-}
prettyRenderAnsiText :: HydratedReport -> Text
prettyRenderAnsiText =
  PPTerminal.renderStrict
    . layoutPretty defaultLayoutOptions
    . alterAnnotations toAnsiAnnotation
    . prettyReport
 where
  toAnsiAnnotation :: ReportAnnotation -> [AnsiStyle]
  toAnsiAnnotation = \case
    Header -> [bold, colorDull Red]
    Context -> [italicized, color Yellow]
    Content -> [colorDull Yellow]

-- | Formats a report in a structured manner.
prettyReport :: HydratedReport -> ReportDoc
prettyReport = vsep . punctuate divider . getHydratedDocs
 where
  divider = vsep [line, pretty $ Text.replicate 20 "-", mempty]