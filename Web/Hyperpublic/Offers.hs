{-# LANGUAGE OverloadedStrings #-}

-- | Wrappers for calling the methods of the Hyperpublic Geo Deals and Events
-- endpoint.
module Web.Hyperpublic.Offers
where

import Data.Aeson ( Value )
import Data.ByteString ( ByteString
                       , append )
import Data.ByteString.Char8 ()

import Network.HTTP.Types ( SimpleQuery )

import Web.Hyperpublic


-- | Call the show method of the Geo Deals and Events endpoint. API
-- documentation at <http://developer.hyperpublic.com/offers/show-an-offer/>
show :: HpAuth      -- ^ API authorization
     -> ByteString  -- ^ The id of the offer to be returned
     -> IO Value    -- ^ JSON output
show auth hpId = callApi auth ("/offers/" `append` hpId) []

-- | Call the find method of the Geo Deals and Events endpoint. API
-- documentation at <http://developer.hyperpublic.com/offers/find-offers/>
find :: HpAuth       -- ^ API authorization
     -> SimpleQuery  -- ^ Query parameters
     -> IO Value     -- ^ JSON output
find auth query = callApi auth "/offers" query
