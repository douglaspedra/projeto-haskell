{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import Data.Text (Text, unpack)
import Data.Functor.Identity
import Data.Function

import Obelisk.Route
import Obelisk.Route.TH

checFullREnc :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
checFullREnc = checkEncoder fullRouteEncoder & 
  \case
    Left err -> error $ unpack err
    Right encoder -> encoder

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  --BackendRoute_Cliente :: BackendRoute ()
  BackendRoute_Barraca :: BackendRoute ()
  BackendRoute_BarracaListar :: BackendRoute ()
  BackendRoute_BarracaBuscar :: BackendRoute Int
  BackendRoute_BarracaEditar :: BackendRoute Int
  BackendRoute_BarracaDeletar :: BackendRoute Int
  BackendRoute_Produto :: BackendRoute ()
  BackendRoute_ProdutoListar :: BackendRoute ()
  BackendRoute_ProdutoBuscar :: BackendRoute Int
  BackendRoute_ProdutoBarraca :: BackendRoute Int
  BackendRoute_ProdutoEditar :: BackendRoute Int
  BackendRoute_ProdutoDeletar :: BackendRoute Int
  BackendRoute_Funcionario :: BackendRoute ()
  BackendRoute_FuncionarioListar :: BackendRoute ()
  BackendRoute_FuncionarioBuscar :: BackendRoute Int
  BackendRoute_FuncionarioEditar :: BackendRoute Int
  BackendRoute_FuncionarioDeletar :: BackendRoute Int

  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      --BackendRoute_Cliente -> PathSegment "cliente" $ unitEncoder mempty
      BackendRoute_Barraca -> PathSegment "barraca" $ unitEncoder mempty
      BackendRoute_Produto -> PathSegment "produto" $ unitEncoder mempty      
      BackendRoute_Funcionario -> PathSegment "funcionario" $ unitEncoder mempty
      --         
      --BackendRoute_ClienteListar -> PathSegment "clienteListar" $ unitEncoder mempty
      BackendRoute_BarracaListar -> PathSegment "barracaListar" $ unitEncoder mempty
      BackendRoute_ProdutoListar -> PathSegment "produtoListar" $ unitEncoder mempty
      BackendRoute_FuncionarioListar -> PathSegment "funcionarioListar" $ unitEncoder mempty
      --
      --BackendRoute_ClienteBuscar -> PathSegment "clienteBuscar" $ readShowEncoder
      BackendRoute_BarracaBuscar -> PathSegment "barracaBuscar" $ readShowEncoder
      BackendRoute_ProdutoBuscar -> PathSegment "produtoBuscar" $ readShowEncoder 
      BackendRoute_FuncionarioBuscar -> PathSegment "funcionarioBuscar" $ readShowEncoder
      BackendRoute_ProdutoBarraca -> PathSegment "produtoBarracaBuscar" $ readShowEncoder

      BackendRoute_BarracaEditar -> PathSegment "barracaEditar" $ readShowEncoder
      BackendRoute_ProdutoEditar -> PathSegment "produtoEditar" $ readShowEncoder 
      BackendRoute_FuncionarioEditar -> PathSegment "funcionarioEditar" $ readShowEncoder

      BackendRoute_BarracaDeletar -> PathSegment "barracaDeletar" $ readShowEncoder
      BackendRoute_ProdutoDeletar -> PathSegment "produtoDeletar" $ readShowEncoder 
      BackendRoute_FuncionarioDeletar -> PathSegment "funcionarioDeletar" $ readShowEncoder

      )
  (\case
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty)

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
