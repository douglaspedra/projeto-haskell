{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
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
import Common.Api
import Data.Aeson.Text

import Database.PostgreSQL.Simple

getConn :: ConnectInfo
getConn = ConnectInfo "ec2-23-23-133-10.compute-1.amazonaws.com"
                      5432 -- porta
                      "zyowwaforpiwvr"
                      "0c81f755d7caace626c9faf7d104f6ea7ced0e79c4e03e92865c3f03eb906afd"
                      "d9c9i9kdg966v1"

migrate :: Query
migrate = "CREATE TABLE IF NOT EXISTS cliente (id SERIAL PRIMARY KEY, nome TEXT NOT NULL)"

migrateBarraca :: Query
migrateBarraca = "CREATE TABLE IF NOT EXISTS barraca (id SERIAL PRIMARY KEY, nome TEXT NOT NULL, categoria Text NOT NULL)"

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
    dbcon <- connect getConn -- essa linha faz conexao com o banco
    serve $ do
      \case
            BackendRoute_BarracaListar :/ () -> method GET $ do
              res :: [Barraca] <- liftIO $ do
                  execute_ dbcon migrateBarraca
                  query_ dbcon "SELECT * FROM barraca"
              modifyResponse $ setResponseStatus 200 "OK"
              writeLazyText (encodeToLazyText res)              
            BackendRoute_BarracaBuscar :/ bid -> method GET $ do
              res :: [Barraca] <- liftIO $ do
                  execute_ dbcon migrateBarraca
                  query dbcon "SELECT * FROM barraca WHERE  id=?" (Only (bid :: Int))
              if res /= [] then do
                  modifyResponse $ setResponseStatus 200 "OK"
                  writeLazyText (encodeToLazyText (Prelude.head res))
              else
                  modifyResponse $ setResponseStatus 404 "NOT FOUND"                
            BackendRoute_Barraca :/ () -> method POST $ do
              barraca <- A.decode <$> readRequestBody 2000
              case barraca of
                Just barr -> do
                  liftIO $ do
                    execute_ dbcon migrateBarraca
                    execute dbcon "INSERT INTO barraca (nome,categoria) VALUES (?,?)"
                            (barracaNome barr, barracaCategoria barr)
                  modifyResponse $ setResponseStatus 200 "OK"
                Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
            BackendRoute_Cliente :/ () -> method POST $ do
              Just nome <- A.decode <$> readRequestBody 2000
              liftIO $ do
                execute_ dbcon migrate
                execute dbcon "INSERT INTO cliente (nome) VALUES (?)" [nome :: Text]
              modifyResponse $ setResponseStatus 200 "OK"
            _ -> return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
