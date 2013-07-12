{-
Copyright (c) 2013, Markus Barenhoff <alios@alios.org>
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the <organization> nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}

module Data.ISO8211.Tree
    ( dsSingleData
    , dsLinearStruct
    , dsMultiDimStruct
    , findRecords
    , findRecord
    , findRecord'
    , findSubRecord
    , findSubRecord'
    , findSubRecords
    , sdRecordField
    , mdRecordField
    , mdRecords
    , maybemdRecords ) where


import           Data.ISO8211.Parser
import qualified Data.Map            as Map
import           Data.Tree


--
-- record navigation
--

dropISORoot :: DataRecord -> DataRecord
dropISORoot r
    | ((fst $ rootLabel r) == "0001") = head . subForest $ r
    | otherwise = error $ "node is not a ISO 8211 Record Identifier:" ++ show r

findRecords :: String -> DataFile -> [DataRecord]
findRecords t (_, rs) =
    filter (\n -> (fst . rootLabel $ n) == t) $ map dropISORoot rs

findRecord' :: String -> DataFile -> Maybe DataRecord
findRecord' t f =
    case (findRecords t f) of
      [] -> Nothing
      (x:_) -> Just x

findRecord :: String -> DataFile -> DataRecord
findRecord t f =
    maybe (error $ "unable to findRecord: " ++ t)
          id $ findRecord' t f

findRecordFieldLS :: String ->  DataRecord -> DataFieldT
findRecordFieldLS t dr =
   let fs = dsLinearStruct . snd . rootLabel $ dr
   in case (t `Map.lookup` fs) of
        Nothing -> error $ "unable to find subfield: " ++ t
        Just f -> f


findSubRecords :: String -> DataRecord -> [DataRecord]
findSubRecords t dr =
    filter (\n -> (fst $ rootLabel n) == t) (subForest dr)


findSubRecord' :: String -> DataRecord -> Maybe DataRecord
findSubRecord' t r =
    case (findSubRecords t r) of
      [] -> Nothing
      (x:_) -> Just x

findSubRecord :: String -> DataRecord -> DataRecord
findSubRecord t r =
    maybe (error $ "unable to findSubRecord: " ++ t)
          id $ findSubRecord' t r

sdRecordField :: DataField t => DataRecord -> String -> t
sdRecordField dr t = fromDataField (findRecordFieldLS t dr)

mdRecords' :: String -> DataRecord -> Maybe [Map.Map String DataFieldT]
mdRecords' t r = maybe Nothing (Just . dsMultiDimStruct . snd . rootLabel)
                 $ findSubRecord' t r

mdRecords :: String -> DataRecord -> [Map.Map String DataFieldT]
mdRecords t r =
    maybe (error $ "unable to find mdRecord: " ++ t) id
              $ mdRecords' t r

mdRecordField :: DataField c => String -> Map.Map String DataFieldT -> c
mdRecordField t m =
    fromDataField . maybe (error $ "unable to find tag: " ++ t) id
      $ t `Map.lookup` m

maybemdRecords ::
  String -> (Map.Map String DataFieldT -> a) -> DataRecord -> [a]
maybemdRecords t c r = maybe [] (map c) $ mdRecords' t r
