{-# LANGUAGE OverloadedStrings #-}

module Web.Hyperpublic.Places
where

import Data.ByteString ( ByteString
                       , append )
import Data.ByteString.Char8 ()

import Web.Hyperpublic

show :: HpAuth -> ByteString -> IO Value
show auth id = callApi auth ("/places/" `append` id) []

find :: HpAuth -> SimpleQuery -> IO Value
find auth query = callApi auth "/places" query
