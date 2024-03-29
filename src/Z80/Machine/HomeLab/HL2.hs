module Z80.Machine.HomeLab.HL2 where

import Z80
import Z80.Utils
import Data.Word
import Data.Char

videoStart :: Word16
videoStart = 0xc001

numCols :: (Num a) => a
numCols = 40

rowstride :: (Num a) => a
rowstride = numCols

printCenteredLine :: Location -> Location -> String -> Z80ASM
printCenteredLine base row s = mdo
    ld IX $ base + numCols * row + (numCols - fromIntegral (length s)) `div` 2
    ld IY text
    text <- stringLoopB s do
        ldVia A [IX] [IY]
        inc IX
        inc IY
    pure ()

printCenteredLines :: Location -> Location -> [String] -> Z80ASM
printCenteredLines base row = mapM_ (uncurry $ printCenteredLine base) . zip [row..]

invert :: String -> String
invert = map invert1

invert1 :: Char -> Char
invert1 = chr . (+ 0x80) . ord
