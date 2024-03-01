module Z80.Machine.TRS80.Defs where

import Z80
import Z80.Utils
import Data.Word
import Data.Bits

videoStart :: Word16
videoStart = 0x3c00

rowstride :: (Num a) => a
rowstride = 64

numLines :: (Num a) => a
numLines = 16

videoSize :: Word16
videoSize = rowstride * numLines
