module Z80.Machine.HomeLab.HTP where

import Z80
import Z80.Utils

import qualified Data.ByteString as BS
import Data.List (intersperse)

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
