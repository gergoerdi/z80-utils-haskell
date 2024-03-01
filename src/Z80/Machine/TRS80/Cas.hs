module Z80.Machine.TRS80.Cas where

import Z80
import Z80.Utils
import qualified Data.ByteString as BS

cas :: BS.ByteString -> ASMBlock -> BS.ByteString
cas fileName prog = mconcat
    [ leader
    , BS.singleton 0xa5
    , BS.singleton 0x55
    , BS.take 6 . (<> BS.replicate 6 0x00) $ fileName
    , mconcat blocks
    , BS.singleton 0x78
    , word start
    ]
  where
    bs = asmData prog
    start = asmOrg prog

    block addr chunk = mconcat
        [ BS.singleton 0x3c
        , BS.singleton $ fromIntegral $ BS.length chunk
        , withCRC $ mconcat
            [ word addr
            , chunk
            ]
        ]
    blocks = zipWith block [start, start + 256 ..] (splitInto 256 bs)

    leader = BS.replicate 256 0x00
    withCRC bs = bs <> (BS.singleton . negate . BS.foldr' (+) 0 $ bs)

    word w = BS.pack [lo, hi]
      where
        (lo, hi) = wordBytes w

splitInto :: Int -> BS.ByteString -> [BS.ByteString]
splitInto len bs
  | BS.null bs = []
  | otherwise = let (bs1, bs2) = BS.splitAt len bs in bs1 : splitInto len bs2
