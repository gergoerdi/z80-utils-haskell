module Z80.Machine.HomeLab.HL34 where

import Z80
import Z80.Utils
import Data.Word

videoStart :: Word16
videoStart = 0xf800

rowstride :: (Num a) => a
rowstride = 64

numLines :: (Num a) => a
numLines = 32

videoSize :: Word16
videoSize = rowstride * numLines

pageIO :: Z80ASM
pageIO = out [0xff] A

pageRAM :: Z80ASM
pageRAM = out [0x7f] A

printA :: Z80ASM
printA = rst 0x28

getKeyA :: Z80ASM
getKeyA = call 0x035b
