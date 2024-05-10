-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.6).

-- Lexer definition for use with Alex 3
{
{-# OPTIONS -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -w #-}

{-# LANGUAGE PatternSynonyms #-}

module Omni.Lex where

import Prelude

import qualified Data.Text
import qualified Data.Bits
import Data.Char     (ord)
import Data.Function (on)
import Data.Word     (Word8)
}

-- Predefined character classes

$c = [A-Z\192-\221] # [\215]  -- capital isolatin1 letter (215 = \times) FIXME
$s = [a-z\222-\255] # [\247]  -- small   isolatin1 letter (247 = \div  ) FIXME
$l = [$c $s]         -- letter
$d = [0-9]           -- digit
$i = [$l $d _ ']     -- identifier character
$u = [. \n]          -- universal: any character

-- Symbols and non-identifier-like reserved words

@rsyms = \( | \) | \: | \= | \, | \- \>

:-

-- Whitespace (skipped)
$white+ ;

-- Symbols
@rsyms
    { tok (eitherResIdent TV) }

-- token InfixOpIdent
[\* \+ \- \/ \< \= \> \? \^]+
    { tok (eitherResIdent T_InfixOpIdent) }

-- Keywords and Ident
$l $i*
    { tok (eitherResIdent TV) }

-- Integer
$d+
    { tok TI }

{
-- | Create a token with position.
tok :: (Data.Text.Text -> Tok) -> (Posn -> Int -> Data.Text.Text -> Token)
tok f p l = PT p l . f

-- | Token without position.
data Tok
  = TK {-# UNPACK #-} !TokSymbol  -- ^ Reserved word or symbol.
  | TL !Data.Text.Text            -- ^ String literal.
  | TI !Data.Text.Text            -- ^ Integer literal.
  | TV !Data.Text.Text            -- ^ Identifier.
  | TD !Data.Text.Text            -- ^ Float literal.
  | TC !Data.Text.Text            -- ^ Character literal.
  | T_InfixOpIdent !Data.Text.Text
  deriving (Eq, Show, Ord)

-- | Smart constructor for 'Tok' for the sake of backwards compatibility.
pattern TS :: Data.Text.Text -> Int -> Tok
pattern TS t i = TK (TokSymbol t i)

-- | Keyword or symbol tokens have a unique ID.
data TokSymbol = TokSymbol
  { tsText :: Data.Text.Text
      -- ^ Keyword or symbol text.
  , tsID   :: !Int
      -- ^ Unique ID.
  } deriving (Show)

-- | Keyword/symbol equality is determined by the unique ID.
instance Eq  TokSymbol where (==)    = (==)    `on` tsID

-- | Keyword/symbol ordering is determined by the unique ID.
instance Ord TokSymbol where compare = compare `on` tsID

-- | Token with position.
data Token
  = PT  Posn Int Tok
  | Err Posn
  deriving (Eq, Show, Ord)

-- | Pretty print a position.
printPosn :: Posn -> String
printPosn (Pn _ l c) = "line " ++ show l ++ ", column " ++ show c

-- | Pretty print the position of the first token in the list.
tokenPos :: [Token] -> String
tokenPos (t:_) = printPosn (tokenPosn t)
tokenPos []    = "end of file"

-- | Get the position of a token.
tokenPosn :: Token -> Posn
tokenPosn (PT posn _len _tok) = posn
tokenPosn (Err posn)  = posn

-- | Get the length of a token.
tokenLen :: Token -> Int
tokenLen (PT _posn len _tok) = len
tokenLen (Err _)  = 0

-- | Get start line and column of a token.
tokenLineCol :: Token -> (Int, Int)
tokenLineCol = posLineCol . tokenPosn

-- | Get end line and column of a token.
tokenLineColEnd :: Token -> (Int, Int)
tokenLineColEnd t = (l, c + n)
  where
    (l, c) = tokenLineCol t
    n = tokenLen t

-- | Get line and column for both start and end of a token.
tokenSpan :: Token -> ((Int, Int), (Int, Int))
tokenSpan t = (tokenLineCol t, tokenLineColEnd t)

-- | Get line and column of a position.
posLineCol :: Posn -> (Int, Int)
posLineCol (Pn _ l c) = (l,c)

-- | Convert a token into "position token" form.
mkPosToken :: Token -> (((Int, Int), Int), Data.Text.Text)
mkPosToken t = ((tokenLineCol t, tokenLen t), tokenText t)

-- | Convert a token to its text.
tokenText :: Token -> Data.Text.Text
tokenText t = case t of
  PT _ _ (TS s _) -> s
  PT _ _ (TL s)   -> Data.Text.pack (show s)
  PT _ _ (TI s)   -> s
  PT _ _ (TV s)   -> s
  PT _ _ (TD s)   -> s
  PT _ _ (TC s)   -> s
  Err _         -> Data.Text.pack "#error"
  PT _ _ (T_InfixOpIdent s) -> s

-- | Convert a token to a string.
prToken :: Token -> String
prToken t = Data.Text.unpack (tokenText t)

-- | Finite map from text to token organized as binary search tree.
data BTree
  = N -- ^ Nil (leaf).
  | B Data.Text.Text Tok BTree BTree
      -- ^ Binary node.
  deriving (Show)

-- | Convert potential keyword into token or use fallback conversion.
eitherResIdent :: (Data.Text.Text -> Tok) -> Data.Text.Text -> Tok
eitherResIdent tv s = treeFind resWords
  where
  treeFind N = tv s
  treeFind (B a t left right) =
    case compare s a of
      LT -> treeFind left
      GT -> treeFind right
      EQ -> t

-- | The keywords and symbols of the language organized as binary search tree.
resWords :: BTree
resWords =
  b ":" 5
    (b "," 3 (b ")" 2 (b "(" 1 N N) N) (b "->" 4 N N))
    (b "Int" 7 (b "=" 6 N N) (b "module" 8 N N))
  where
  b s n = B bs (TS bs n)
    where
    bs = Data.Text.pack s

-- | Unquote string literal.
unescapeInitTail :: Data.Text.Text -> Data.Text.Text
unescapeInitTail = Data.Text.pack . unesc . tail . Data.Text.unpack
  where
  unesc s = case s of
    '\\':c:cs | elem c ['\"', '\\', '\''] -> c : unesc cs
    '\\':'n':cs  -> '\n' : unesc cs
    '\\':'t':cs  -> '\t' : unesc cs
    '\\':'r':cs  -> '\r' : unesc cs
    '\\':'f':cs  -> '\f' : unesc cs
    '"':[]       -> []
    c:cs         -> c : unesc cs
    _            -> []

-------------------------------------------------------------------
-- Alex wrapper code.
-- A modified "posn" wrapper.
-------------------------------------------------------------------

data Posn = Pn !Int !Int !Int
  deriving (Eq, Show, Ord)

alexStartPos :: Posn
alexStartPos = Pn 0 1 1

alexMove :: Posn -> Char -> Posn
alexMove (Pn a l c) '\t' = Pn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (Pn a l c) '\n' = Pn (a+1) (l+1)   1
alexMove (Pn a l c) _    = Pn (a+1)  l     (c+1)

type Byte = Word8

type AlexInput = (Posn,     -- current position,
                  Char,     -- previous char
                  [Byte],   -- pending bytes on the current char
                  Data.Text.Text)   -- current input string

tokens :: Data.Text.Text -> [Token]
tokens str = go (alexStartPos, '\n', [], str)
    where
      go :: AlexInput -> [Token]
      go inp@(pos, _, _, str) =
               case alexScan inp 0 of
                AlexEOF                   -> []
                AlexError (pos, _, _, _)  -> [Err pos]
                AlexSkip  inp' len        -> go inp'
                AlexToken inp' len act    -> act pos len (Data.Text.take len str) : (go inp')

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p, c, (b:bs), s) = Just (b, (p, c, bs, s))
alexGetByte (p, _, [], s) =
  case Data.Text.uncons s of
    Nothing  -> Nothing
    Just (c,s) ->
             let p'     = alexMove p c
                 (b:bs) = utf8Encode c
              in p' `seq` Just (b, (p', c, bs, s))

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p, c, bs, s) = c

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
  where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
}
