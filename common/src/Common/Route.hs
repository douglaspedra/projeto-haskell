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
  BackendRoute_Cliente :: BackendRoute ()
  BackendRoute_Barraca :: BackendRoute ()
  BackendRoute_BarracaListar :: BackendRoute ()
  BackendRoute_BarracaBuscar :: BackendRoute Int
  BackendRoute_Produto :: BackendRoute ()
  BackendRoute_ProdutoListar :: BackendRoute ()
  BackendRoute_ProdutoBuscar :: BackendRoute Int
  BackendRoute_Oferta :: BackendRoute ()
  BackendRoute_OfertaListar :: BackendRoute ()
  BackendRoute_OfertaBuscar :: BackendRoute Int
  BackendRoute_Funcionario :: BackendRoute ()
  BackendRoute_FuncionarioListar :: BackendRoute ()
  BackendRoute_FuncionarioBuscar :: BackendRoute Int

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
      BackendRoute_Cliente -> PathSegment "cliente" $ unitEncoder mempty
      BackendRoute_Barraca -> PathSegment "barraca" $ unitEncoder mempty
      BackendRoute_Oferta -> PathSegment "oferta" $ unitEncoder mempty   
      BackendRoute_Produto -> PathSegment "produto" $ unitEncoder mempty      
      BackendRoute_Funcionario -> PathSegment "funcionario" $ unitEncoder mempty
      --         
      --BackendRoute_ClienteListar -> PathSegment "clienteListar" $ unitEncoder mempty
      BackendRoute_BarracaListar -> PathSegment "barracaListar" $ unitEncoder mempty
      BackendRoute_OfertaListar -> PathSegment "ofertaListar" $ unitEncoder mempty
      BackendRoute_ProdutoListar -> PathSegment "produtoListar" $ unitEncoder mempty
      BackendRoute_FuncionarioListar -> PathSegment "funcionarioListar" $ unitEncoder mempty
      --
      --BackendRoute_ClienteBuscar -> PathSegment "clienteBuscar" $ readShowEncoder
      BackendRoute_BarracaBuscar -> PathSegment "barracaBuscar" $ readShowEncoder
      BackendRoute_OfertaBuscar -> PathSegment "ofertaBuscar" $ readShowEncoder   
      BackendRoute_ProdutoBuscar -> PathSegment "produtoBuscar" $ readShowEncoder 
      BackendRoute_FuncionarioBuscar -> PathSegment "funcionarioBuscar" $ readShowEncoder 
      )
  (\case
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty)

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
