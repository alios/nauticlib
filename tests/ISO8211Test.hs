module Main(main) where

import Data.S57.ISO8211
import Data.Attoparsec(parseOnly)
import qualified Data.ByteString as BS

main = do
 r <- fmap (parseOnly parseDataFile ) $ BS.readFile "/home/alios/tmp/US5TX51M.000"
 case r of
   Left err -> print err
   Right (ddr, drs) -> do
          print ddr
          print $ drs 

