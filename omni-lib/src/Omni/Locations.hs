module Omni.Locations (
  -- * Classes
  Located (..),

  -- * Types
  Locs,
  Loc (..),
  SrcLoc (..),
  SrcRange (..),
  SrcPos (..),

  -- * Smart constructors
  fromList,
  singleLoc,
  convertPos,
  generated,
) where

import Data.Set (Set)
import Data.Set qualified as Set
import Omni.Abs qualified as Parsed
import Prettyprinter

-- | Things that have a location in the source code.
class Located a where
  getLoc :: a -> Loc

-- | A collection of unique locations.
newtype Locs = Locs {unwrapLocs :: Set Loc}
  deriving (Semigroup, Monoid, Show, Eq, Ord)

fromList :: [Loc] -> Locs
fromList = Locs . Set.fromList

singleLoc :: Loc -> Locs
singleLoc = Locs . Set.singleton

generated :: Loc
generated = Generated

instance Pretty Locs where
  pretty = align . sep . map pretty . Set.toList . unwrapLocs

-- | A location of a part of the code.
data Loc = Generated | Src SrcLoc
  deriving (Eq, Ord, Show)

instance Pretty Loc where
  pretty Generated = "(generated)"
  pretty (Src loc) = pretty loc

-- | A location in the source code.
data SrcLoc = Loc
  { sourceFile :: FilePath
  , range :: SrcRange
  }
  deriving (Show, Eq, Ord)

instance Pretty SrcLoc where
  pretty (Loc{sourceFile, range}) = mconcat [pretty sourceFile, ":", pretty range]

-- | A range of characters inside a source file.
data SrcRange = SrcRange
  { rangeFrom :: SrcPos
  , rangeTo :: SrcPos
  }
  deriving (Show, Eq, Ord)

instance Pretty SrcRange where
  pretty (SrcRange{rangeFrom, rangeTo}) = mconcat [pretty rangeFrom, "-", pretty rangeTo]

-- | An absolute character position inside a source file.
data SrcPos = SrcPos
  { posLine :: Int
  , posColumn :: Int
  }
  deriving (Show, Eq, Ord)

instance Pretty SrcPos where
  pretty (SrcPos{posLine, posColumn}) = mconcat [pretty posLine, ":", pretty posColumn]

-- | Converts a parser-position into a source location.
convertPos :: FilePath -> Parsed.BNFC'Position -> Loc
convertPos _ Nothing = Generated
convertPos file (Just ((startLine, startCol), (endLine, endCol))) =
  Src $
    Loc
      { sourceFile = file
      , range =
          SrcRange
            { rangeFrom =
                SrcPos
                  { posLine = startLine
                  , posColumn = startCol
                  }
            , rangeTo =
                SrcPos
                  { posLine = endLine
                  , posColumn = endCol
                  }
            }
      }
