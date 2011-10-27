{-# LANGUAGE OverloadedStrings #-}

-- | Wrappers for calling the methods of the Hyperpublic Categories endpoint.
module Web.Hyperpublic.Categories
( show
, find
) where

import Prelude hiding ( show )

import Data.Aeson ( Value )
import Data.ByteString ( ByteString
                       , append )
import Data.ByteString.Char8 ()

import Web.Hyperpublic
import Web.Hyperpublic.Internal


-- | Call the show method of the Categories endpoint. API documentation at
-- <http://developer.hyperpublic.com/categories/show-category/>
show :: HpAuth      -- ^ API authorization
     -> ByteString  -- ^ The id of the category to be returned
     -> IO Value    -- ^ JSON output
show auth hpId = callApi auth ("/categories/" `append` hpId) []

-- | Call the find method of the Categories endpoint, which simply lists the
-- full category hierarchy. (The endpoint does not currently support more
-- specific queries.)
find :: HpAuth       -- ^ API authorization
     -> IO Value     -- ^ JSON output
find auth = callApi auth "/categories" []
