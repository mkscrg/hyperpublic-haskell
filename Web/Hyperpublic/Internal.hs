{-# LANGUAGE OverloadedStrings #-}

module Web.Hyperpublic.Internal
( callApi
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
                          , StdMethod (..)
                          , renderStdMethod
                          , simpleQueryToQuery )

import Web.Hyperpublic


callApi :: HpAuth -> StdMethod -> ByteString -> SimpleQuery -> IO Value
callApi auth mthd urlPath query = do
    let query' = [ ("client_id", clientId auth)
                 , ("client_secret", clientSecret auth) ] ++
                 filter cleanQuery query
    rsp <- withManager (httpLbsRedirect $ request mthd urlPath query')
    eitherToIO $ Right rsp >>= readResponse >>= eitherResult . parse json
  where
    cleanQuery (k, _) = k /= "client_id" && k /= "client_secret"

request :: StdMethod -> ByteString -> SimpleQuery -> Request m
request mthd urlPath query = def
    { method = renderStdMethod mthd
    , host = "api.hyperpublic.com"
    , path = "/api/v1" `append` urlPath
    , queryString = simpleQueryToQuery query
    , port = 443
    , secure = True }

readResponse :: Response -> Either String ByteString
readResponse rsp =
    let strictBody = foldr1 append $ Lazy.toChunks $ responseBody rsp
    in if statusCode rsp == 200
      then Right strictBody
      else Left $ show (statusCode rsp) ++ ": " ++ unpack strictBody

eitherToIO :: Either String a -> IO a
eitherToIO (Right a) = return a
eitherToIO (Left str) = fail str
