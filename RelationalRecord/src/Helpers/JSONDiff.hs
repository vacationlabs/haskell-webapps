{-# LANGUAGE OverloadedStrings #-}

module  Helpers.JSONDiff where

import  Data.Aeson.Types
import  Data.Aeson                      (encode)
import  qualified Data.HashMap.Strict   as HM
import  Data.Vector                     as V (fromList)
import  Data.ByteString.Lazy            as BL (ByteString, empty)
import  Data.Maybe                      (fromMaybe)


-- creates a row-wise diff JSON object for two JSON objects of the same type.
-- changed attributes will appear like:
--      {"someAttr": [oldVal, newVal]}
-- NB. it's hardcoded that a possible timestamp attribute "updatedAt"
--       is left out of the resulting diff
jsonDiff :: ToJSON a => a -> a -> ByteString
jsonDiff old_ new_ =
    case (toJSON old_, toJSON new_) of
        (Object old, Object new) -> encode $ Object $ HM.fromList $
            [ (k, Array $ V.fromList [fromMaybe Null v_, v])                    -- keys with a different value in the new object
                | (k, v) <- HM.toList new
                , isNoTimeStamp k
                , let v_ = HM.lookup k old
                , v_ /= Just v
                ]
            ++ [ (k, Array $ V.fromList [v_, Null])                             -- keys present in old, but absent in new object
                | (k, v_) <- HM.toList old
                , isNoTimeStamp k
                , not $ k `HM.member` new
                ]
        _ -> BL.empty
  where
    isNoTimeStamp = (/= "updatedAt")
