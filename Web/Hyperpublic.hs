{-# LANGUAGE OverloadedStrings #-}

module Web.Hyperpublic
( HpAuth
, callHpApi
) where

import Data.Aeson ( Value
                  , json )
import Data.Attoparsec ( eitherResult
                       , parse )
import Data.ByteString ( ByteString
                       , append )
import Data.ByteString.Char8 ( unpack )
import qualified Data.ByteString.Lazy as Lazy
import Network.HTTP.Enumerator ( Request (..)
                               , Response (..)
                               , def
                               , httpLbsRedirect
                               , withManager )
import Network.HTTP.Types ( SimpleQuery
                          , simpleQueryToQuery )


callHpApi :: HpAuth -> ByteString -> SimpleQuery -> IO Value
callHpApi auth path query = do
    let query' = [ ("client_id", clientId auth)
                 , ("client_secret", clientSecret auth) ] ++
                 filter cleanQuery query
    rsp <- withManager (httpLbsRedirect $ hpRequest path query')
    eitherToIO $ Right rsp >>= readResponse >>= eitherResult . parse json
  where
    cleanQuery (k, _) = k /= "client_id" && k /= "client_secret"

data HpAuth = HpAuth
    { clientId :: ByteString
    , clientSecret :: ByteString
    } deriving (Show)


hpRequest :: ByteString -> SimpleQuery -> Request m
hpRequest path query = def
    { host = "api.hyperpublic.com"
    , path = "/api/v1" `append` path
    , queryString = simpleQueryToQuery query
    , port = 443
    , secure = True }

readResponse :: Response -> Either String ByteString
readResponse rsp =
    let strictBody = foldr1 append $ Lazy.toChunks $ responseBody rsp
    in if statusCode rsp == 200
      then Right strictBody
      else Left $ unpack strictBody

eitherToIO :: Either String a -> IO a
eitherToIO (Right a) = return a
eitherToIO (Left str) = fail str
