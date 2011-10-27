{-# LANGUAGE OverloadedStrings #-}

-- | Wrappers for calling the methods of the Hyperpublic Places+ endpoint.
module Web.Hyperpublic.Places
( show
, find
) where

import Prelude hiding ( show )

import Data.Aeson ( Value )
import Data.ByteString ( ByteString
                       , append )
import Data.ByteString.Char8 ()

import Network.HTTP.Types ( SimpleQuery
                          , StdMethod (..) )

import Web.Hyperpublic
import Web.Hyperpublic.Internal


-- | Call the show method of the Places+ endpoint. API documentation at
-- <http://developer.hyperpublic.com/places/show-a-place/>
show :: HpAuth      -- ^ API authorization
     -> ByteString  -- ^ The id of the place to be returned
     -> IO Value    -- ^ JSON output
show auth hpId = callApi auth GET ("/places/" `append` hpId) []

-- | Call the find method of the Places+ endpoint. API documentation at
-- <http://developer.hyperpublic.com/offers/find-offers/>
find :: HpAuth       -- ^ API authorization
     -> SimpleQuery  -- ^ Query parameters
     -> IO Value     -- ^ JSON output
find auth query = callApi auth GET "/places" query
