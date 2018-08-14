{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import System.IO

import Compiler (compileProg)
import Types (CompileError(..))

-- * api

type Api = "compile" :> ReqBody '[JSON] String :> Post '[JSON] String

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
server = compile

compile :: String -> Handler String
compile rawProg = case (compileProg rawProg "main") of
    Right regex -> return regex
    Left NoMainMatcher -> return "Error: no main matcher"
    Left (ParseErrors errors) -> return $ "Error: parsing error: " ++ (show errors)
