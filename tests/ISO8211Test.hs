{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import           Data.Attoparsec  (parseOnly)
import qualified Data.ByteString  as BS
import           Text.Show.Pretty (ppShow)

import qualified Data.ISO8211     as ISO8211
import qualified Data.S57         as S57
import           Data.S57Conduit

import           Filesystem.Path
import           Prelude          hiding (FilePath)

import           Data.Conduit

enc_root :: FilePath
enc_root = "/home/alios/src/nauticlib/test_data/ENC_ROOT"

fn :: FilePath
--fn = enc_root </> "US5FL12M/US5FL12M.001"
--fn = enc_root </> "CATALOG.031"

fn = enc_root </> "US5FL98M" </> "US5FL98M.002"

main = do
  df <- isoFileIO fn
--  r <-  (yield df $$ x )
  putStr . ppShow $ df

main2 :: IO ()
main2 = do
 df <- dataFileIO fn
 let filter_map =  S57.df_frids
 putStr . ppShow . filter_map $ df
