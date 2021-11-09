{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.


menu :: DomBuilder t m => m()
menu = do
  elAttr "nav" ("class" =: "navbar navbar-dark bg-dark") $ do
    elAttr "div" ("class" =: "container-fluid") $ do
      elAttr "a" ("class" =: "navbar-brand" 
                <> "href" =: "#") (text "i-Feira")
      elAttr "button" ("class" =: "navbar-toggler" 
                    <> "type" =: "button" 
                    <> "data-bs-toggle" =: "collapse" 
                    <> "data-bs-target" =: "#navbarNav" 
                    <> "aria-controls" =: "navbarNav" 
                    <> "aria-expanded" =: "false" 
                    <> "aria-label" =: "Toggle navigation") $ do
        elAttr "span" ("class" =: "navbar-toggler-icon") blank

    elAttr "div" ("class" =: "collapse navbar-collapse" 
               <> "id" =: "navbarNav") $ do
      elAttr "ul" ("class" =: "navbar-nav") $ do
        elAttr "li" ("class" =: "nav-item") $ do
          elAttr "a" ("class" =: "nav-link active" 
                   <> "aria-current" =: "page" 
                   <> "href" =: "#") 
                      (text "Pagina Inicial")
        elAttr "li" ("class" =: "nav-item") $ do
          elAttr "a" ("class" =: "nav-link" <> "href" =: "#") 
                      (text "Barracas de Feira")
        elAttr "li" ("class" =: "nav-item") $ do
          elAttr "a" ("class" =: "nav-link" <> "href" =: "#") 
                      (text "Categorias")







frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "i-Feira"
      elAttr "link" ("href" =: static @"main.css" 
                  <> "type" =: "text/css" 
                  <> "rel" =: "stylesheet") blank
      elAttr "link" ("href" =: "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css" 
                  <> "rel" =: "stylesheet" 
                  <> "integrity" =: "sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC" 
                  <> "crossorigin" =: "anonymous") blank
      elAttr "script" ("href" =: "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/js/bootstrap.bundle.min.js" 
                  <> "integrity" =: "sha384-MrcW6ZMFYlzcLA8Nl+NtUVF0sA7MsXsP1UyJoMp4YLEuNSfAP+JcXn/tWtIaxVXM" 
                  <> "crossorigin" =:"anonymous") blank



  , _frontend_body = do
      el "h1" $ text "Trabalho P2 - Haskell"
      el "p" $ text "Grupo: Douglas C. Pedra | Gabriel | Mariana | Thales"
      menu
      elAttr "img" ("src" =: static @"agricultor.jpg") blank
      el "p" $ text $ T.pack commonStuff
      
      -- `prerender` and `prerender_` let you choose a widget to run on the server
      -- during prerendering and a different widget to run on the client with
      -- JavaScript. The following will generate a `blank` widget on the server and
      -- print "Hello, World!" on the client.
      prerender_ blank $ liftJSM $ void $ eval ("console.log('Hello, World!')" :: T.Text)

--      elAttr "img" ("src" =: static @"obelisk.jpg") blank
      el "div" $ do
        exampleConfig <- getConfig "common/example"
        case exampleConfig of
          Nothing -> text "No config file found in config/common/example"
          Just s -> text $ T.decodeUtf8 s
      return ()
  }