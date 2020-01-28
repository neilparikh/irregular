{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import Data.Aeson.Types

import System.IO
import GHC.Generics

import Compiler (compileProg)
import Types (CompileError(..))
import Util (matchText)

-- * api

type Api = "match" :> ReqBody '[JSON] RequestBody :> Post '[JSON] String

api :: Proxy Api
api = Proxy

-- * app

main :: IO ()
main = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ logStdoutDev $ corsWithContentType $ serve api server

corsWithContentType :: Middleware
corsWithContentType = cors (const $ Just policy)
    where
      policy = simpleCorsResourcePolicy
        { corsRequestHeaders = ["Content-Type"] }

server :: Server Api
server = match

match :: RequestBody -> Handler String
match (RequestBody program' text') = return $ case (compileProg program' "main") of
    Right regex -> case (matchText regex (replaceNewline text') "<mark>" "</mark>") of
        Nothing -> text'
        Just highlightedString -> highlightedString
    Left NoMainMatcher -> "Error: no main matcher"
    Left (ParseErrors errors) -> "Error: parsing error: " ++ (show errors)
    Left CyclicDefn -> "Error: cyclic definitions are not allowed"
    where
    replaceNewline ('\n':xs) = "<br>" ++ (replaceNewline  xs)
    replaceNewline (x:xs) = x:(replaceNewline xs)
    replaceNewline [] = []

data RequestBody = RequestBody {
    program :: String,
    text :: String
} deriving (Generic)

instance FromJSON RequestBody
