{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Api.Photo (PhotoAPI, photoHandlers) where

import Control.Monad.Trans.Except
import Servant

type PhotoAPI = "photos" :>
                (Capture "path_segment_1" String :>
                 Capture "path_segment_2" String :>
                 Capture "geometry_or_style" String :>
                 Capture "original_filename" String :>
                    Get '[JSON] String)

photoGet :: String -> String -> String -> String -> ExceptT ServantErr IO String
photoGet path1 path2 geometry filename = return "return a photo"

photoHandlers = photoGet
