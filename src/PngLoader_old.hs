{-# LANGUAGE OverloadedStrings #-}
module PngLoader where

import System.IO
import System.Environment
import qualified Data.ByteString as B
import qualified Data.Word as W
import qualified Data.ByteString.Char8 as C
import qualified Codec.Compression.Zlib as Zlib
import qualified Data.ByteString.Lazy as LB
import qualified Data.List as L
import Text.Printf
import Data.List.Split


--Debugging stuff
import System.Directory
import Data.Char

type RGBPixel = [W.Word8]

test_file = "C:/Users/andrew/haskell/Image/sample.png"
png_file = B.readFile test_file

png_header_str = "\137PNG\r\n\SUB\n"
png_header_bytestring = C.pack png_header_str

test_bpp = 3

data Chunk =
  Misc_Chunk {dataLength :: Int,
              headerStr :: [Char],
              chunkData :: [W.Word8],
              crc :: [W.Word8]
             } |
  IHDR_Chunk {dataLength :: Int,
              headerStr :: [Char],
              chunkData :: [W.Word8],
              crc :: [W.Word8],
              width :: Int,
              height :: Int,
              bpp :: Int
             } |
  IDAT_Chunk {dataLength :: Int,
              headerStr :: [Char],
              chunkData :: [W.Word8],
              crc :: [W.Word8],
              width :: Int,
              height :: Int,
              bpp :: Int
             } deriving (Show)

data FilterFlag = NoneF | SubF | UpF | AvgF | PaethF deriving (Show)

data PngImg = PngImg {ihdr :: Chunk,
                      idat :: Chunk,
                      misc_chunks :: [Chunk]
                      }

data Image = Image {imWidth :: Int, imHeight :: Int, imData :: [RGBPixel]}

loadPng :: B.ByteString -> [[RGBPixel]]
loadPng img_bytestring = decodeIDATs (height ihdr) . filter isIDAT $ chunks
  where
    chunks = extractChunks . B.drop 8 $ img_bytestring
    ihdr = getIHDR chunks

isIDAT :: Chunk -> Bool
isIDAT chunk = headerStr chunk == "IDAT"


getIHDR :: [Chunk] -> Chunk
getIHDR (chunk:chunks) = if (headerStr chunk == "IHDR")
                         then chunk
                         else getIHDR chunks

extractChunks :: B.ByteString -> [Chunk]
extractChunks bs = extractChunksHelper bs []
      where
   extractChunksHelper bytestring chunk_list
        | B.null bytestring = reverse chunk_list
        | otherwise         = extractChunksHelper newByteString (chunk:chunk_list)
         where
           header_str     = C.unpack . B.take 4 . B.drop 4 $ bytestring
           data_len      = bytes_to_number . B.unpack . B.take 4 $ bytestring
           chunk_len     = 12 + data_len
           newByteString = B.drop chunk_len bytestring
           chunk_data    = B.unpack . B.take data_len . B.drop 8 $ bytestring
           crc_data      = B.unpack . B.take 4 . B.drop (8+data_len) $ bytestring
           chunk         =  case header_str of
             "IHDR" -> (IHDR_Chunk data_len header_str chunk_data crc_data
                        cwidth cheight test_bpp)
                 where cwidth  = bytes_to_number . take 4 $ chunk_data
                       cheight = bytes_to_number . take 4 . drop 4 $ chunk_data
             "IDAT" -> (IDAT_Chunk data_len header_str chunk_data crc_data
                        cwidth cheight test_bpp)
                 where cwidth  = width . head . reverse  $ chunk_list
                       cheight = height . head . reverse $ chunk_list
             _      -> Misc_Chunk data_len header_str chunk_data crc_data


decodeIDATs :: Int -> [Chunk] -> [[RGBPixel]]
decodeIDATs h idats =
  let
    unzipped_word8 = LB.unpack . Zlib.decompress . LB.pack
                   .  concat . map chunkData $ idats
    line_width = (length unzipped_word8) `div` h
    getFlag (x:xs) = (wordToFlag x, xs)
  in
      map (chunksOf test_bpp) . unfilterLines
    . map getFlag . chunksOf line_width $ unzipped_word8


unfilterLines :: [(FilterFlag, [W.Word8])] -> [[W.Word8]]
unfilterLines flaglines = unfilter flaglines []
  where
    unfilter [] filtered = filtered
    unfilter ((NoneF,line):lines) filtered = unfilter lines (line:filtered)
    unfilter ((SubF,line):lines) filtered = unfilter lines (newline:filtered)
           where
             newline = zipWith (+) line (0:0:0:newline)
    unfilter ((UpF,line):lines) a@(prevLine:filtered) = unfilter lines (newline:a)
           where
             newline = zipWith (+) line prevLine
    unfilter ((UpF,line):lines) [] = unfilter lines [line]


flipHorizontal :: [[RGBPixel]] -> [[RGBPixel]]
flipHorizontal = map reverse

flipVertical :: [[RGBPixel]] -> [[RGBPixel]]
flipVertical = reverse


--simpleFilter



saveAsPPM img_word8 outPath =
  writeFile outPath $ (printf "P3\n%d %d\n255\n" w h) ++ flat_img
  where
    flat_img = L.intercalate "\n" . map show . concat . concat . reverse $ img_word8
    h = length img_word8
    w = length . head $ img_word8

-- Helper function
bytes_to_number :: [W.Word8] -> Int
bytes_to_number =
  foldl (\n (i,a) -> n + (fromIntegral a)*256^i) 0 . zip [0,1..] . reverse

testMain :: IO ()
testMain = do
  argv <- getArgs
  img_bs <- B.readFile $ head argv
  let flipped = flipVertical . loadPng $ img_bs
  saveAsPPM flipped  (argv !! 1)

wordToFlag :: W.Word8 -> FilterFlag
wordToFlag word = case word of
  0 -> NoneF
  1 -> SubF
  2 -> UpF
  3 -> AvgF
  4 -> PaethF
  otherwise -> error "Invalid filter flag"
