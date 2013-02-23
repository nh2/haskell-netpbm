{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, MultiParamTypeClasses, TemplateHaskell #-}


-- | Parsing the netpbm image formates (PBM, PGM and PPM, both ASCII and binary) from 'ByteString's.
--
-- To parse one of these formats, use `parsePPM`.
--
-- Currently, only P6 images are implemented.
-- Implementing the other types should be straighforward.
module Graphics.Netpbm (
  PPMType (..)
, PPM (..)
, PpmPixelRGB8
, PpmPixelRGB16
, PPMHeader (..)
, PpmPixelData (..)
, parsePPM
, PpmParseResult
-- TODO expose attoparsec functions in .Internal package
) where

import           Control.Monad
import           Control.Applicative
import           Data.Attoparsec.ByteString as A
import           Data.Attoparsec.ByteString.Char8 as A8
import           Data.Attoparsec.Binary (anyWord16be)
import           Data.ByteString (ByteString)
import           Data.Char (ord)
import           Data.List (foldl')
import           Data.Word (Word8, Word16)
import           Foreign.Storable.Record as Store
import           Foreign.Storable (Storable (..))

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable

import Data.Vector.Unboxed.Deriving


-- | The netpbm image type of an image.
data PPMType = P1 -- ^ ASCII bitmap
             | P2 -- ^ ASCII greymap
             | P3 -- ^ ASCII pixmap (color)
             | P4 -- ^ binary bitmap
             | P5 -- ^ binary greymap
             | P6 -- ^ binary pixmap (color)
             deriving (Eq, Show, Enum, Ord)


-- | A PPM file with type, dimensions, and image data.
data PPM = PPM {
  ppmHeader :: PPMHeader
, ppmData   :: PpmPixelData
}

data PPMHeader = PPMHeader {
  ppmType   :: PPMType
, ppmWidth  :: Int
, ppmHeight :: Int
} deriving (Eq, Show)

instance Show PPM where
  show PPM { ppmHeader = PPMHeader { ppmType, ppmWidth, ppmHeight } } = "PPM " ++ show ppmType ++ " image " ++ dim
    where
      dim = show (ppmWidth, ppmHeight)


-- | A pixel containing three 8-bit color components, RGB.
data PpmPixelRGB8 = PpmPixelRGB8 {-# UNPACK #-} !Word8 -- Red
                                 {-# UNPACK #-} !Word8 -- Green
                                 {-# UNPACK #-} !Word8 -- Blue
                                 deriving (Eq, Show)

-- | A pixel containing three 16-bit color components, RGB.
data PpmPixelRGB16 = PpmPixelRGB16 {-# UNPACK #-} !Word16 -- Red
                                   {-# UNPACK #-} !Word16 -- Green
                                   {-# UNPACK #-} !Word16 -- Blue
                                   deriving (Eq, Show)

-- | Image data, either 8 or 16 bits.
data PpmPixelData = PpmPixelDataRGB8 (U.Vector PpmPixelRGB8)   -- ^ For 8-bit PPMs.
                  | PpmPixelDataRGB16 (U.Vector PpmPixelRGB16) -- ^ For 16-bit PPMs.


-- * Unbox instance for pixels

derivingUnbox "PpmPixelRGB8"
    [t| PpmPixelRGB8 -> (Word8, Word8, Word8) |]
    [| \ (PpmPixelRGB8 a b c) -> (a, b, c) |]
    [| \ (a, b, c) -> PpmPixelRGB8 a b c |]

derivingUnbox "PpmPixelRGB16"
    [t| PpmPixelRGB16 -> (Word16, Word16, Word16) |]
    [| \ (PpmPixelRGB16 a b c) -> (a, b, c) |]
    [| \ (a, b, c) -> PpmPixelRGB16 a b c |]


-- * Storable instance for pixels

storePixel8 :: Store.Dictionary PpmPixelRGB8
storePixel8 =
  Store.run $ liftA3 PpmPixelRGB8
    (Store.element (\(PpmPixelRGB8 x _ _) -> x))
    (Store.element (\(PpmPixelRGB8 _ y _) -> y))
    (Store.element (\(PpmPixelRGB8 _ _ z) -> z))

storePixel16 :: Store.Dictionary PpmPixelRGB16
storePixel16 =
  Store.run $ liftA3 PpmPixelRGB16
    (Store.element (\(PpmPixelRGB16 x _ _) -> x))
    (Store.element (\(PpmPixelRGB16 _ y _) -> y))
    (Store.element (\(PpmPixelRGB16 _ _ z) -> z))

instance Storable PpmPixelRGB8 where
  sizeOf = Store.sizeOf storePixel8
  alignment = Store.alignment storePixel8
  peek = Store.peek storePixel8
  poke = Store.poke storePixel8

instance Storable PpmPixelRGB16 where
  sizeOf = Store.sizeOf storePixel16
  alignment = Store.alignment storePixel16
  peek = Store.peek storePixel16
  poke = Store.poke storePixel16


-- | Parses a netpbm magic number.
-- One of P1, P2, P3, P4, P5, P6.
magicNumberParser :: Parser PPMType
magicNumberParser = do
  magic <- choice ["P1", "P2", "P3", "P4", "P5", "P6"]
  case magic of
    "P1" -> return P1
    "P2" -> return P2

    "P3" -> return P3
    "P4" -> return P4
    "P5" -> return P5
    "P6" -> return P6
    _    -> fail $ "PPM: uknown PPM format " ++ show magic



-- | Parses a SINGLE PPM file.
--
-- Specification: http://netpbm.sourceforge.net/doc/ppm.html
--
-- There can be multiple images in one file, each starting with
-- a "Pn" magic number.
--
-- Comments starting with '#' can only be
-- "before the whitespace character that delimits the raster"
-- (see http://netpbm.sourceforge.net/doc/pbm.html).
-- Nevertheless, I interpret that as "comments cannot be
-- inside the magic number".
--
-- See also the notes for `imagesParser`.
ppmParser :: Parser PPM
ppmParser = do
  ppmType <- magicNumberParser
  -- TODO Implement the other netpbm image types
  when (ppmType /= P6) $ error "haskell-netpbm currently only supports PPM P6"
  comments
  skipSpace
  comments
  width <- decimalC
  comments
  skipSpace
  comments
  height <- decimalC
  comments
  skipSpace
  comments
  maxColorVal <- decimalC
  when (not $ isValidColorVal maxColorVal) $
    fail $ "PPM: invalid color maxval " ++ show maxColorVal
  comments
  _ <- A8.satisfy isSpace -- obligatory SINGLE whitespace
  -- Starting from here, comments are not allowed any more
  raster <- if maxColorVal < 256 -- 1 or 2 bytes per pixel
      then PpmPixelDataRGB8 <$> (U.replicateM (height * width) $
             PpmPixelRGB8 <$> anyWord8 <*> anyWord8 <*> anyWord8)
      else PpmPixelDataRGB16 <$> (U.replicateM (height * width) $
             PpmPixelRGB16 <$> anyWord16be <*> anyWord16be <*> anyWord16be)

  return $ PPM (PPMHeader ppmType width height) raster

  where
    isValidColorVal v = v > 0 && v < 65536
    comments = void $ many comment
    comment = "#" .*> A.takeWhile isNotNewline <* endOfLine
    isNotNewline w = w /= 10 && w /= 13
    -- Decimal, possibly with comments interleaved,
    -- but starting and ending with a digit.
    -- See the notes about comments above.
    decimalC :: Parser Int
    decimalC = foldl' shiftDecimalChar 0 <$> ((:) <$> digit <*> many (comments *> digit))
    shiftDecimalChar a d = a * 10 + ord d - (48 :: Int)


-- | Parses a full PPM file, containing one or more images.
--
-- "A PPM file consists of a sequence of one or more PPM images."
-- We allow trailing whitespace after images, which is AGAINST THE SPEC:
--
-- >"A PPM file consists of a sequence of one or more PPM images.
-- > There are no data, delimiters, or padding before, after, or between images."
--
-- However, you can find PPM files that have trailing whitespace, especially a '\n'.
imagesParser :: Parser [PPM]
imagesParser = many1 (ppmParser <* skipSpace)


-- | The result of a PPM parse.
--
-- See `parsePPM`.
type PpmParseResult = Either String ([PPM], Maybe ByteString)


-- | Parses a PPM file from the given 'ByteString'.
-- On failure, @Left error@ contains the error message.
-- On success, @Right (images, Maybe rest)@ contains the parsed images
-- and potentially an unparsable rest input.
parsePPM :: ByteString -> PpmParseResult
parsePPM bs = case parse imagesParser bs of
  Done ""   images -> Right (images, Nothing)
  Done rest images -> Right (images, Just rest)
  Fail _ _ e       -> Left e
  -- The image file ByteStrings are not terminated by '\0',
  -- so Attoparsec will issue a Partial result when it
  -- parses to EOF. Passing in "" signalizes EOF.
  Partial cont -> case cont "" of
    Done ""   images -> Right (images, Nothing)
    Done rest images -> Right (images, Just rest)
    Partial _        -> error "parsePPM bug: Got a partial result after end of input"
    Fail _ _ e       -> Left e
