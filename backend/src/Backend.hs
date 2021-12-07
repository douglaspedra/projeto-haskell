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
migrateBarraca = "CREATE TABLE IF NOT EXISTS tb_barraca (idBarraca SERIAL PRIMARY KEY, nome TEXT NOT NULL, categoria Text NOT NULL)"

migrateProduto :: Query
migrateProduto = "CREATE TABLE IF NOT EXISTS tb_produto (idProduto SERIAL PRIMARY KEY, cdBarraca INTEGER NOT NULL REFERENCES tb_barraca ON DELETE CASCADE, nome TEXT NOT NULL, categoriaProduto TEXT NOT NULL, valor REAL NOT NULL, qtd INTEGER NOT NULL)"

migrateOferta :: Query
migrateOferta = "CREATE TABLE IF NOT EXISTS tb_oferta (idOferta SERIAL PRIMARY KEY, cdProduto INTEGER NOT NULL REFERENCES tb_produto ON DELETE CASCADE, desconto REAL NOT NULL, valor REAL NOT NULL)"

migrateFuncionario :: Query
migrateFuncionario = "CREATE TABLE IF NOT EXISTS tb_funcionario (idFuncionario SERIAL PRIMARY KEY, cdBarraca INTEGER NOT NULL REFERENCES tb_barraca ON DELETE CASCADE, nome TEXT NOT NULL, cpf TEXT NOT NULL, telefone TEXT NOT NULL)"


backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
    dbcon <- connect getConn -- essa linha faz conexao com o banco
    serve $ do
      \case
            BackendRoute_FuncionarioListar :/ () -> method GET $ do
              res :: [Funcionario] <- liftIO $ do
                  execute_ dbcon migrateFuncionario
                  query_ dbcon "SELECT * FROM tb_funcionario"
              modifyResponse $ setResponseStatus 200 "OK"
              writeLazyText (encodeToLazyText res)            
            BackendRoute_OfertaListar :/ () -> method GET $ do
              res :: [Oferta] <- liftIO $ do
                  execute_ dbcon migrateOferta
                  query_ dbcon "SELECT * FROM tb_oferta"
              modifyResponse $ setResponseStatus 200 "OK"
              writeLazyText (encodeToLazyText res)              
            BackendRoute_ProdutoListar :/ () -> method GET $ do
              res :: [Produto] <- liftIO $ do
                  execute_ dbcon migrateProduto
                  query_ dbcon "SELECT * FROM tb_produto"
              modifyResponse $ setResponseStatus 200 "OK"
              writeLazyText (encodeToLazyText res)             
            BackendRoute_BarracaListar :/ () -> method GET $ do
              res :: [Barraca] <- liftIO $ do
                  execute_ dbcon migrateBarraca
                  query_ dbcon "SELECT * FROM tb_barraca"
              modifyResponse $ setResponseStatus 200 "OK"
              writeLazyText (encodeToLazyText res)          
----------------------------------------------------------------------------------------------------------                            
            BackendRoute_FuncionarioBuscar :/ fid -> method GET $ do
              res :: [Funcionario] <- liftIO $ do
                  execute_ dbcon migrateFuncionario
                  query dbcon "SELECT * FROM tb_funcionario WHERE id=?" (Only (fid :: Int))
              if res /= [] then do
                  modifyResponse $ setResponseStatus 200 "OK"
                  writeLazyText (encodeToLazyText (Prelude.head res))
              else
                  modifyResponse $ setResponseStatus 404 "NOT FOUND"             
            BackendRoute_OfertaBuscar :/ oid -> method GET $ do
              res :: [Oferta] <- liftIO $ do
                  execute_ dbcon migrateOferta
                  query dbcon "SELECT * FROM tb_oferta WHERE id=?" (Only (oid :: Int))
              if res /= [] then do
                  modifyResponse $ setResponseStatus 200 "OK"
                  writeLazyText (encodeToLazyText (Prelude.head res))
              else
                  modifyResponse $ setResponseStatus 404 "NOT FOUND"            
            BackendRoute_ProdutoBuscar :/ pid -> method GET $ do
              res :: [Produto] <- liftIO $ do
                  execute_ dbcon migrateProduto
                  query dbcon "SELECT * FROM tb_produto WHERE id=?" (Only (pid :: Int))
              if res /= [] then do
                  modifyResponse $ setResponseStatus 200 "OK"
                  writeLazyText (encodeToLazyText (Prelude.head res))
              else
                  modifyResponse $ setResponseStatus 404 "NOT FOUND"            
            BackendRoute_BarracaBuscar :/ bid -> method GET $ do
              res :: [Barraca] <- liftIO $ do
                  execute_ dbcon migrateBarraca
                  query dbcon "SELECT * FROM tb_barraca WHERE id=?" (Only (bid :: Int))
              if res /= [] then do
                  modifyResponse $ setResponseStatus 200 "OK"
                  writeLazyText (encodeToLazyText (Prelude.head res))
              else
                  modifyResponse $ setResponseStatus 404 "NOT FOUND"
----------------------------------------------------------------------------------------------------------                                  
            BackendRoute_Funcionario :/ () -> method POST $ do
              funci <- A.decode <$> readRequestBody 2000
              case funci of
                Just funcionarios -> do
                  liftIO $ do
                    execute_ dbcon migrateFuncionario
                    execute dbcon "INSERT INTO tb_funcionario (cdBarraca, nome, cpf, telefone) VALUES (?,?,?,?)"
                            (cdBarracaFuncionario funcionarios, nomeFuncionario funcionarios, cpfFuncionario funcionarios, telefoneFuncionario funcionarios)
                  modifyResponse $ setResponseStatus 200 "OK"
                Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"            
            BackendRoute_Oferta :/ () -> method POST $ do
              ofer <- A.decode <$> readRequestBody 2000
              case ofer of
                Just ofertas -> do
                  liftIO $ do
                    execute_ dbcon migrateOferta
                    execute dbcon "INSERT INTO tb_oferta (cdProduto, desconto, valor) VALUES (?,?,?)"
                            (cdProdutoOferta ofertas, descontoOferta ofertas, valorOferta ofertas)
                  modifyResponse $ setResponseStatus 200 "OK"
                Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
            BackendRoute_Produto :/ () -> method POST $ do
              prod <- A.decode <$> readRequestBody 2000
              case prod of
                Just produtos -> do
                  liftIO $ do
                    execute_ dbcon migrateProduto
                    execute dbcon "INSERT INTO tb_produto (cdBarraca, nome, valor, qtd) VALUES (?,?,?,?)"
                            (cdBarracaProduto produtos, nomeProduto produtos, valorProduto produtos, qtdProduto produtos)
                  modifyResponse $ setResponseStatus 200 "OK"
                Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
            BackendRoute_Barraca :/ () -> method POST $ do
              barraca <- A.decode <$> readRequestBody 2000
              case barraca of
                Just barr -> do
                  liftIO $ do
                    execute_ dbcon migrateBarraca
                    execute dbcon "INSERT INTO tb_barraca (nome,categoria) VALUES (?,?)"
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
