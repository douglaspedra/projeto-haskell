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

--data Cliente = Cliente Text deriving (Generic, ToJSON, FromJSON)

data Funcionario = Funcionario {
    idFuncionario :: Int,
    cdBarracaFuncionario :: Int,
    nomeFuncionario :: Text,
    cpfFuncionario :: Text,
    telefoneFuncionario :: Text
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)


data Barraca = Barraca {
    barracaId :: Int,
    barracaNome :: Text,
    barracaCategoria :: Text
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

data Produto = Produto {
    idProduto :: Int,
    cdBarracaProduto :: Int,
    nomeProduto :: Text,
    categoriaProduto :: Text,
    valorProduto :: Double,
    qtdProduto :: Int
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

data Oferta = Oferta {
    idOferta :: Int,
    cdProdutoOferta :: Int,
    descontoOferta :: Double,
    valorOferta :: Double    
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

-- data Zona = Zona {
--     idZona :: Int,
--     numeroZona :: Int,
--     diaZona :: Text,
--     avenidaZona :: Text,
--     bairroZona :: Text,
--     cidadeZona :: Text
-- } deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)



{- module Common.Api where

commonStuff :: String
commonStuff = "Here is a string defined in Common.Api" -}
