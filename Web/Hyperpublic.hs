-- | A thin wrapper for the Hyperpublic API, which uses @http-enumerator@ to
-- fetch results and @aeson@ to return them as JSON.
--
-- Full API documentation can be found at <http://developer.hyperpublic.com/>.
--
-- Some basic usage examples follow. This code is included in the distribution as @Examples.hs@, buildable as the @hyperpublic-example@ executable by turning on the @example@ flag. Usage questions may be posted on the Hyperpublic API Developers mailing list at <http://groups.google.com/group/hyperpublic-api-developers>.
--
-- >{-# LANGUAGE OverloadedStrings #-}
-- >
-- >module Main where
-- >
-- >import Data.Aeson
-- >import Data.ByteString.Char8 ()
-- >import qualified Data.Map as M
-- >import Data.Maybe
-- >import Data.Text
-- >import qualified Data.Vector as V
-- >
-- >import Web.Hyperpublic
-- >import qualified Web.Hyperpublic.Places as Places
-- >import qualified Web.Hyperpublic.Offers as Offers
-- >
-- >-- Sequence the two examples.
-- >main :: IO ()
-- >main = placeNamesNearHq >> offerDescr
-- >
-- >-- Find places near Hyperpublic HQ and print the name of each.
-- >placeNamesNearHq :: IO ()
-- >placeNamesNearHq =
-- >    let json = Places.find auth [( "address"
-- >                                 , "416 W 13th St, New York, NY 10014" )]
-- >    in json >>= putStrLn . show . getNames
-- >  where
-- >    getNames (Array arr) = mapMaybe getName $ V.toList arr
-- >    getName (Object obj) = getTextField obj "display_name"
-- >
-- >-- Find the offer with id 4e90567c297a200001008db9 and print its description.
-- >offerDescr :: IO ()
-- >offerDescr =
-- >    let json = Offers.show auth "4e90567c297a200001008db9"
-- >    in json >>= putStrLn . show . getDescr
-- >  where
-- >    getDescr (Object obj) = maybe "" id $ getTextField obj "description"
-- >
-- >-- Create an authorization record. Get your own credentials at
-- >-- http://www.hyperpublic.com/registerapi
-- >auth :: HpAuth
-- >auth = HpAuth { clientId = "8UufhI6bCKQXKMBn7AUWO67Yq6C8RkfD0BGouTke"
-- >              , clientSecret = "zdoROY5XRN0clIWsEJyKzHedSK4irYee8jpnOXaP" }
-- >
-- >-- Extract a text field from an 'Data.Aeson.Object'.
-- >getTextField :: Object -> Text -> Maybe Text
-- >getTextField obj txt = M.lookup txt obj >>= resultToMaybe . fromJSON
-- >  where
-- >    resultToMaybe (Success a) = Just a
-- >    resultToMaybe (Error _) = Nothing
module Web.Hyperpublic
( HpAuth (..)
) where

import Data.ByteString ( ByteString )


-- | A record for passing around API authorization credentials.
data HpAuth = HpAuth
    { clientId :: ByteString
    , clientSecret :: ByteString
    } deriving (Show)
