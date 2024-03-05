module Z80.Machine.HomeLab.HTP where

import Z80
import Z80.Utils

import qualified Data.ByteString as BS
import Data.List (intersperse)
import Data.Word
import Data.Bits
import Control.Monad
import Codec.Audio.Wave

htp :: BS.ByteString -> [ASMBlock] -> BS.ByteString
htp label blocks = mconcat $
    intersperse (BS.singleton 0x01) records <> [BS.singleton 0x00]
  where
    leader = BS.replicate 256 0x00
    records = zipWith record (label:repeat mempty) blocks

    record label block = mconcat
        [ leader
        , BS.singleton 0xa5
        , label
        , BS.singleton 0x00
        , word $ asmOrg block
        , word . fromIntegral $ BS.length bs
        , bs
        , crc bs
        ]
      where
        bs = asmData block

    crc = BS.singleton . BS.foldr' (+) 0

    word w = BS.pack [lo, hi]
      where
        (lo, hi) = wordBytes w

renderToWav :: FilePath -> BS.ByteString -> IO ()
renderToWav fileName htp = writeWaveFile fileName wav writeData
  where
    wav = Wave
        { waveFileFormat = WaveVanilla
        , waveSampleRate = 44_100
        , waveSampleFormat = SampleFormatPcmInt 8
        , waveChannelMask = speakerMono
        , waveDataOffset = 0
        , waveDataSize = 0
        , waveSamplesTotal = 0
        , waveOtherChunks = mempty
        }

    low = 0x7f
    high = 0xf0

    bitLength = 1600
    space = bitLength - sync
    sync = 150

    writeData h = do
        silence h 2000
        level h sync high
        mapM_ (writeBit h) $ concatMap bitsOf $ BS.unpack htp
        silence h 2000

    writeBit h = \case
        True -> replicateM_ 2 $ spike h $ (space - sync) `div` 2
        False -> spike h space

    spike h lead = do
        level h lead low
        level h sync high

    silence h len = level h len 0x7f

    level h len val = replicateM_ (byteCount len) $ BS.hPut h $ BS.singleton val
      where
        usecSampleLength = 1_000_000 `div` waveSampleRate wav
        byteCount usecs = fromIntegral $ usecs `div` usecSampleLength

    bitsOf :: Word8 -> [Bool]
    bitsOf byte = [testBit byte (7 - i) | i <- [0..7]]
