module Omni.Reporting (
  -- * Types
  Report,
  Reports,
  HydratedReport,

  -- * Warnings

  -- ** Constructors
  shadowedIdentifier,

  -- * Errors

  -- ** Constructors
  fileLoad,
  moduleNotFound,
  duplicatedModule,
  parse,
  duplicateIdentifier,
  unknownIdentifier,
  noLLVMIRModules,
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
import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Omni.Config (Directory, OptimizationLevel)
import Omni.Locations
import Omni.Name qualified as Name
import Omni.Query (Query)
import Omni.TypeCheck.L00AST qualified as L0
import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), bold, color, colorDull, italicized)
import Prettyprinter.Render.Terminal qualified as PPTerminal
import Prettyprinter.Render.Text qualified as PPText
import Rock (Task)

-- | Collection of compiler outputs.
type Reports = Seq Report

-- | Compiler output.
data Report = Warn Warning | Err Error

-- Warnings -----------------------------------------------

-- | Non-critical information about the compilation.
data Warning = ShadowedIdentifier L0.GenericName L0.GenericName

shadowedIdentifier :: (L0.ToGenericName n1, L0.ToGenericName n2) => n1 -> n2 -> Report
shadowedIdentifier shadower shadowee =
  Warn $
    ShadowedIdentifier
      (L0.toGeneric shadower)
      (L0.toGeneric shadowee)

-- Errors -------------------------------------------------

-- | Critical failures in the compilation.
data Error
  = -- File errors
    FileLoad FilePath IOException
  | ModuleNotFound Name.ModuleName
  | DuplicatedModule Name.ModuleName [FilePath]
  | -- Parse errors
    Parse FilePath String
  | DuplicateIdentifier Text Locs
  | UnknownIdentifier Text Locs
  | -- LLVM errors
    NoLLVMIRModules Directory
  | Linking [String] Lazy.ByteString
  | Optimizing OptimizationLevel FilePath Lazy.ByteString
  | ExecutableGeneration FilePath Lazy.ByteString
  deriving (Show)

fileLoad :: FilePath -> IOException -> Report
fileLoad path = Err . FileLoad path

moduleNotFound :: Name.ModuleName -> Report
moduleNotFound = Err . ModuleNotFound

duplicatedModule :: Name.ModuleName -> [FilePath] -> Report
duplicatedModule name = Err . DuplicatedModule name

parse :: FilePath -> String -> Report
parse path = Err . Parse path

duplicateIdentifier :: Text -> Locs -> Report
duplicateIdentifier name locs = Err $ DuplicateIdentifier name locs

unknownIdentifier :: (L0.ToGenericName a) => a -> Report
unknownIdentifier (L0.toGeneric -> (L0.GenName loc name)) =
  Err $ UnknownIdentifier name (singleLoc loc)

noLLVMIRModules :: Directory -> Report
noLLVMIRModules = Err . NoLLVMIRModules

linking :: [String] -> Lazy.ByteString -> Report
linking args = Err . Linking args

optimizing :: OptimizationLevel -> FilePath -> Lazy.ByteString -> Report
optimizing level path = Err . Optimizing level path

executableGeneration :: FilePath -> Lazy.ByteString -> Report
executableGeneration path = Err . ExecutableGeneration path

-- Pretty-printing ----------------------------------------

-- | Extra data associated with a report document.
data ReportAnnotation
  = ErrorHeader
  | WarningHeader
  | Extra
  | Content

-- | Report document describing e.g. compiler errors or warnings.
type ReportDoc = Doc ReportAnnotation

-- | A report which has been supplemented with additional context.
newtype HydratedReport = Hydrated {getHydratedDocs :: Seq ReportDoc}

-- | Supplements the given errors with additional context.
hydrate :: Reports -> Task Query HydratedReport
hydrate reports = Hydrated <$> mapM hydrateReport reports
 where
  hydrateReport :: Report -> Task Query ReportDoc
  hydrateReport = \case
    Warn warn -> hydrateWarning warn
    Err err -> hydrateError err

  hydrateWarning :: Warning -> Task Query ReportDoc
  hydrateWarning = \case
    ShadowedIdentifier (L0.GenName shadowerLoc shadower) (L0.GenName shadoweeLoc _) ->
      return $
        warningDoc
          "shadowed identifier: "
          ( sep
              [ annotate Content $
                  hcat
                    [ "'"
                    , pretty shadower
                    , "' at "
                    , pretty shadowerLoc
                    ]
              , annotate Content $
                  hcat
                    [ "shadows the identifier at "
                    , pretty shadoweeLoc
                    ]
              ]
          )
   where
    warningDoc :: ReportDoc -> ReportDoc -> ReportDoc
    warningDoc header =
      headerAndBody
        WarningHeader
        Extra
        Content
        (sep ["warning:", header])

  hydrateError :: Error -> Task Query ReportDoc
  hydrateError = \case
    FileLoad path exc ->
      return $
        errorDoc
          ("failed to load file '" <> pretty path <> "'")
          (pretty $ show exc)
    ModuleNotFound name ->
      return $
        errorDoc
          ("unknown module name '" <> pretty name <> "'")
          mempty
    DuplicatedModule name paths ->
      return $
        errorDoc
          ("module named '" <> pretty name <> "' already exists:")
          (vsep $ map pretty paths)
    Parse path err ->
      return $
        errorDoc
          ("failed to parse file '" <> pretty path <> "'")
          (pretty err)
    DuplicateIdentifier name locs ->
      return $
        errorDoc
          ("duplicate identifier '" <> pretty name <> "' at " <> pretty locs)
          mempty
    UnknownIdentifier name locs ->
      return $
        errorDoc
          ("unknown identifier '" <> pretty name <> "' at " <> pretty locs)
          mempty
    NoLLVMIRModules binDir ->
      return $
        errorDocExtras
          "no LLVM IR modules were generated"
          "generating an executable requires at least one IR module"
          (pretty binDir)
    Linking args output ->
      return $
        errorDocExtras
          "linking error:"
          (hsep $ map pretty args)
          (processOutput Content output)
    Optimizing level inputPath output ->
      return $
        errorDocExtras
          "optimizing error:"
          (hsep [pretty $ show level, pretty inputPath])
          (processOutput Content output)
    ExecutableGeneration inputPath output ->
      return $
        errorDocExtras
          "executable generation error:"
          (pretty inputPath)
          (processOutput Content output)
   where
    errorDoc :: ReportDoc -> ReportDoc -> ReportDoc
    errorDoc header = errorDocExtras header mempty

    errorDocExtras :: ReportDoc -> ReportDoc -> ReportDoc -> ReportDoc
    errorDocExtras header =
      headerAndBodyExtras
        ErrorHeader
        Extra
        Content
        (sep ["error:", header])

  headerAndBody ::
    ReportAnnotation ->
    ReportAnnotation ->
    ReportAnnotation ->
    ReportDoc ->
    ReportDoc ->
    ReportDoc
  headerAndBody headerAnnotation extraAnnotation bodyAnnotation header =
    headerAndBodyExtras headerAnnotation extraAnnotation bodyAnnotation header mempty

  headerAndBodyExtras ::
    ReportAnnotation ->
    ReportAnnotation ->
    ReportAnnotation ->
    ReportDoc ->
    ReportDoc ->
    ReportDoc ->
    ReportDoc
  headerAndBodyExtras headerAnnotation extraAnnotation bodyAnnotation header extras body =
    sep
      [ annotate headerAnnotation header
      , indent
          2
          ( sep
              [ annotate extraAnnotation extras
              , indent 2 (annotate bodyAnnotation body)
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
    ErrorHeader -> [bold, colorDull Red]
    WarningHeader -> [bold, colorDull Yellow]
    Extra -> [italicized, color Yellow]
    Content -> [colorDull Yellow]

-- | Formats a report in a structured manner.
prettyReport :: HydratedReport -> ReportDoc
prettyReport = vsep . punctuate divider . toList . getHydratedDocs
 where
  divider = vsep [line, pretty $ Text.replicate 20 "-", mempty]