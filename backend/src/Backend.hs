{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase, GADTs #-}

module Backend where

import Common.Route
import Obelisk.Backend
import Database.PostgreSQL.Simple
import Data.Text
import Obelisk.Route
import Snap.Core
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A

import Database.PostgreSQL.Simple

getConn :: ConnectInfo
getConn = ConnectInfo "ec2-23-23-133-10.compute-1.amazonaws.com"
                      5432 -- porta
                      "zyowwaforpiwvr"
                      "0c81f755d7caace626c9faf7d104f6ea7ced0e79c4e03e92865c3f03eb906afd"
                      "d9c9i9kdg966v1"

migrate :: Query
migrate = "CREATE TABLE IF NOT EXISTS cliente (id SERIAL PRIMARY KEY, nome TEXT NOT NULL)"

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
    dbcon <- connect getConn -- essa linha faz conexao com o banco
    serve $ do
      \case
            BackendRoute_Cliente :/ () -> do
              Just nome <- A.decode <$> readRequestBody 2000
              liftIO $ do
                execute_ dbcon migrate
                execute dbcon "INSERT INTO cliente (nome) VALUES (?)" [nome :: Text]
              modifyResponse $ setResponseStatus 200 "OK"
            _ -> return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
