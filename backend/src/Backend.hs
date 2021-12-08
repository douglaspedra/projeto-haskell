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
migrateBarraca = "CREATE TABLE IF NOT EXISTS tb_barraca (idBarraca SERIAL PRIMARY KEY, nome TEXT NOT NULL, categoriaBarraca Text NOT NULL)"

migrateProduto :: Query
migrateProduto = "CREATE TABLE IF NOT EXISTS tb_produto (idProduto SERIAL PRIMARY KEY, cdBarraca INTEGER NOT NULL REFERENCES tb_barraca ON DELETE CASCADE, nome TEXT NOT NULL, categoriaProduto TEXT NOT NULL, valorProduto REAL NOT NULL, qtd INTEGER NOT NULL)"

migrateFuncionario :: Query
migrateFuncionario = "CREATE TABLE IF NOT EXISTS tb_funcionario (idFuncionario SERIAL PRIMARY KEY, cdBarraca INTEGER NOT NULL REFERENCES tb_barraca ON DELETE CASCADE, nome TEXT NOT NULL, cpf TEXT NOT NULL, telefone TEXT NOT NULL)"

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
    dbcon <- connect getConn -- essa linha faz conexao com o banco
    serve $ do
      \case
            BackendRoute_FuncionarioDeletar :/ fid -> do
                res :: [Funcionario] <- liftIO $ do
                        execute_ dbcon migrateFuncionario
                        query dbcon "DELETE FROM tb_funcionario where idFuncionario = ?" (Only (fid :: Int))
                if res /= [] then do
                        modifyResponse $ setResponseStatus 200 "OK"   
                        writeLazyText (encodeToLazyText (Prelude.head res))
                else 
                        modifyResponse $ setResponseStatus 404 "NOT FOUND"            

            BackendRoute_ProdutoDeletar :/ pid -> do
                res :: [Produto] <- liftIO $ do
                        execute_ dbcon migrateProduto
                        query dbcon "DELETE FROM tb_produto where idProduto = ?" (Only (pid :: Int))
                if res /= [] then do
                        modifyResponse $ setResponseStatus 200 "OK"   
                        writeLazyText (encodeToLazyText (Prelude.head res))
                else 
                        modifyResponse $ setResponseStatus 404 "NOT FOUND"

            BackendRoute_BarracaDeletar :/ bid -> do
                res :: [Barraca] <- liftIO $ do
                        execute_ dbcon migrateBarraca
                        query dbcon "DELETE FROM tb_barraca where idBarraca = ?" (Only (bid :: Int))
                if res /= [] then do
                        modifyResponse $ setResponseStatus 200 "OK"   
                        writeLazyText (encodeToLazyText (Prelude.head res))
                else 
                        modifyResponse $ setResponseStatus 404 "NOT FOUND"

-------------------------------------------------------------------------------------------------------------------
            
            BackendRoute_FuncionarioEditar :/ fid -> method POST $ do
              funci <- A.decode <$> readRequestBody 2000
              case funci of
                Just funcionario -> do
                  liftIO $ do
                    execute_ dbcon migrateFuncionario
                    execute dbcon "UPDATE tb_funcionario SET nome = ?, cpf = ?, telefone = ? WHERE idFuncionario=?" (nomeFuncionario funcionario, cpfFuncionario funcionario, telefoneFuncionario funcionario, fid)
                  modifyResponse $ setResponseStatus 200 "OK"
                Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"

            BackendRoute_ProdutoEditar :/ pid -> method POST $ do
              prod <- A.decode <$> readRequestBody 2000
              case prod of
                Just produto -> do
                  liftIO $ do
                    execute_ dbcon migrateProduto
                    execute dbcon "UPDATE tb_produto SET nome = ?, categoriaProduto = ?, valorProduto = ?, qtd = ? WHERE idProduto=?" (nomeProduto produto, categoriaProduto produto, valorProduto produto, qtdProduto produto, pid)
                  modifyResponse $ setResponseStatus 200 "OK"
                Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"

            BackendRoute_BarracaEditar :/ bid -> method POST $ do
              barr <- A.decode <$> readRequestBody 2000
              case barr of
                Just barraca -> do
                  liftIO $ do
                    execute_ dbcon migrateBarraca
                    execute dbcon "UPDATE tb_barraca SET nome = ?, categoria = ?, WHERE idBarraca=?" (barracaNome barraca, barracaCategoria barraca, bid)
                  modifyResponse $ setResponseStatus 200 "OK"
                Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"


----------------------------------------------------------------------------------------------------------------            

            BackendRoute_FuncionarioListar :/ () -> method GET $ do
              res :: [Funcionario] <- liftIO $ do
                  execute_ dbcon migrateFuncionario
                  query_ dbcon "SELECT * FROM tb_funcionario"
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
                  query dbcon "select idFuncionario, tb_funcionario.nome, tb_barraca.nome, cpf, telefone from tb_funcionario inner join tb_barraca on tb_barraca.barracaId = tb_funcionario.cdBarraca and tb_funcionario.cdBarraca = ?" (Only (fid :: Int))
              if res /= [] then do
                  modifyResponse $ setResponseStatus 200 "OK"
                  writeLazyText (encodeToLazyText (Prelude.head res))
              else
                  modifyResponse $ setResponseStatus 404 "NOT FOUND"             
            BackendRoute_ProdutoBarraca :/ bid -> method GET $ do
              res :: [Produto] <- liftIO $ do
                  execute_ dbcon migrateProduto
                  query dbcon "SELECT * FROM tb_produto WHERE cdBarraca=?" (Only (bid :: Int))
              if res /= [] then do
                  modifyResponse $ setResponseStatus 200 "OK"
                  writeLazyText (encodeToLazyText res)
              else
                  modifyResponse $ setResponseStatus 404 "NOT FOUND"             
            BackendRoute_ProdutoBuscar :/ pid -> method GET $ do
              res :: [Produto] <- liftIO $ do
                  execute_ dbcon migrateProduto
                  query dbcon "SELECT idProduto, cdBarraca, nome, categoriaProduto, valor, qtd FROM tb_produto WHERE cdBarraca = ?" (Only (pid :: Int))
              if res /= [] then do
                  modifyResponse $ setResponseStatus 200 "OK"
                  writeLazyText (encodeToLazyText (Prelude.head res))
              else
                  modifyResponse $ setResponseStatus 404 "NOT FOUND"            
            BackendRoute_BarracaBuscar :/ bid -> method GET $ do
              res :: [Barraca] <- liftIO $ do
                  execute_ dbcon migrateBarraca
                  query dbcon "SELECT * FROM tb_barraca WHERE idBarraca=?" (Only (bid :: Int))
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
            BackendRoute_Produto :/ () -> method POST $ do
              prod <- A.decode <$> readRequestBody 2000
              case prod of
                Just produtos -> do
                  liftIO $ do
                    execute_ dbcon migrateProduto
                    execute dbcon "INSERT INTO tb_produto (cdBarraca, nome, categoriaProduto, valorProduto, qtd) VALUES (?,?,?,?,?)"
                            (cdBarracaProduto produtos, nomeProduto produtos, categoriaProduto produtos, valorProduto produtos, qtdProduto produtos)
                  modifyResponse $ setResponseStatus 200 "OK"
                Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
            BackendRoute_Barraca :/ () -> method POST $ do
              barr <- A.decode <$> readRequestBody 2000
              case barr of
                Just barraca -> do
                  liftIO $ do
                    execute_ dbcon migrateBarraca
                    execute dbcon "INSERT INTO tb_barraca (nome,categoria) VALUES (?,?)"
                            (barracaNome barraca, barracaCategoria barraca)
                  modifyResponse $ setResponseStatus 200 "OK"
                Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
  , _backend_routeEncoder = fullRouteEncoder
  }
