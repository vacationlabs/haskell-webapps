{-# LANGUAGE FlexibleContexts #-}

module Util where

import Database.HDBC 

mkData1 f (x1:[]) = Just $ f (fromSql x1)
mkData1 _ _ = Nothing
mkData2 f (x1:x2:[]) = Just $ f (fromSql x1) (fromSql x2)
mkData2 _ _ = Nothing
mkData6 f (x1:x2:x3:x4:x5:x6:[]) = Just $ f (fromSql x1) (fromSql x2) (fromSql x3) (fromSql x4) (fromSql x5) (fromSql x6)
mkData9 f (x1:x2:x3:x4:x5:x6:x7:x8:x9:[]) = Just $ f (fromSql x1) (fromSql x2) (fromSql x3) (fromSql x4) (fromSql x5) (fromSql x6) (fromSql x7) (fromSql x8) (fromSql x9)
mkData9 _ _ = Nothing
mkData11 f (x1:x2:x3:x4:x5:x6:x7:x8:x9:x10:x11:[]) = Just $ f (fromSql x1) (fromSql x2) (fromSql x3) (fromSql x4) (fromSql x5) (fromSql x6) (fromSql x7) (fromSql x8) (fromSql x9) (fromSql x10) (fromSql x11)
mkData11 _ _ = Nothing




