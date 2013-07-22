module Main(main) where

import           Data.Attoparsec  (parseOnly)
import qualified Data.ByteString  as BS
import           Text.Show.Pretty (ppShow)

import qualified Data.ISO8211     as ISO8211
import qualified Data.S57         as S57


enc_root :: FilePath
enc_root = "/home/alios/src/nauticlib/test_data/ENC_ROOT/"

fn :: FilePath
fn = enc_root ++ "US5FL12M/US5FL12M.000"
--fn = enc_root ++ "CATALOG.031"

main :: IO ()
main = do
 r <- fmap (parseOnly ISO8211.parseDataFile) $ BS.readFile fn
 case r of
   Left err -> print err
   Right df -> do
          let filter_map = S57.df_frids
          putStr . ppShow . filter_map . S57.s57dataFile $ df
