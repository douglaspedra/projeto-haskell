{-# LANGUAGE DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Common.Api where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple

--Definicoes de tabela
-- data User = User {
--     userId :: Int,
--     userUsername :: Text,
--     userSenha :: Text
-- } deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

data Cliente = Cliente Text deriving (Generic, ToJSON, FromJSON)

-- data Cliente = Cliente {
--     clienteId :: Int,
--     clienteUser :: Int,
--     clienteNome :: Text,
--     clienteCpf :: Text,
--     clienteTelefone :: Text,
--     clienteEndereco :: Text,
--     clienteZona :: Int
-- } deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

-- data Carrinho = Carrinho {
--     carrinhoId :: Int,
--     carrinhoCliente :: Int, --clienteId
--     carrinhoProduto :: Int, --produtoId
--     carrinhoQtd :: Int,
--     carrinhoTotal :: Double
-- } deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

data Barraca = Barraca {
    barracaId :: Int,
--    barracaUser :: Int,
    barracaNome :: Text,
    barracaCategoria :: Text --categoriaId
--    barracaZona :: Int --zonaId
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

-- data Categoria = Categoria {
--     categoriaId :: Int,
--     categoriaTipo :: Text
-- } deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

-- data Zona = Zona {
--     zonaId :: Int,
--     zonaAvenida :: Text,
--     zonaBairro :: Text,
--     zonaCidade :: Text
-- } deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

data Produto = Produto {
    produtoId :: Int,
    produtoBarraca :: Int, --barrcaId
    produtoNome :: Text,
    produtoValor :: Double,
    produtoQtd :: Int
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

data Oferta = Oferta {
    ofertaId :: Int,
    ofertaProduto :: Text,
    ofertaDesconto :: Double,
    ofertaValor :: Double,
    ofertaBarraca :: Int --barracaId
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)





{- module Common.Api where

commonStuff :: String
commonStuff = "Here is a string defined in Common.Api" -}
