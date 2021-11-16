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
import Data.Map (Map)
import Text.Read
import Data.Maybe

import Reflex.Dom.Core

import Common.Api
import Common.Route


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.


--Exemplos aula:

caixas :: (DomBuilder t m, PostBuild t m) => m ()
caixas = el "div" $ do
  t <- inputElement def -- m (Dynamic Text)
  s <- inputElement def -- m (Dynamic Text)
  text " "
  dynText $ zipDynWith (<>) (_inputElement_value t) (_inputElement_value s)


listaAtr :: Map T.Text T.Text
listaAtr = "class" =: "class1" <> "id" =: "li2"

menuEx :: DomBuilder t m => m ()
menuEx = do
  el "div" $ do
    el "ul" $ do
      el "li" (text "Item 1")
      elAttr "li" listaAtr (text "Item 2")
      el "li" (text "Item 3")
      el "li" (text "Item 4")

numberInput :: DomBuilder t m => m (Dynamic t Double)
numberInput = do 
  n <- inputElement $ def
    & inputElementConfig_initialValue .~ "0"
    & inputElementConfig_elementConfig
    . elementConfig_initialAttributes .~ ("type" =: "number")
  return $ fmap (fromMaybe 0 . readMaybe . T.unpack)
              (_inputElement_value n)

caixaSoma :: (DomBuilder t m, PostBuild t m) => m ()
caixaSoma = do
  n1 <- numberInput -- m (Dynamic t Double)
  text " "
  n2 <- numberInput -- m (Dynamic t Double)
  dynText (fmap (T.pack . show) (zipDynWith (+) n1 n2))

revText :: T.Text -> T.Text
revText t = T.pack (reverse (T.unpack t))

buttonClick :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m (Event t T.Text)
buttonClick = do
  t <- inputElement def
  (e,_) <- el' "button" (text "OK")
  return $ attachPromptlyDynWith const
                                  (fmap revText (_inputElement_value t))
                                  (domEvent Click e)

bttnEvt :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m ()
bttnEvt = do
  evt <- buttonClick
  texto <- holdDyn "" evt --Event -> Dynamic
  el "div" (dynText texto)

sumButton :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m (Event t Double)
sumButton = do
  n1 <- numberInput
  text " "
  n2 <- numberInput
  text " "
  (e,_) <- el' "button" (text "OK")
  let dynDouble = zipDynWith (+) n1 n2
  return $ attachPromptlyDynWith const
                                dynDouble
                                (domEvent Click e)

sumEvt :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m ()
sumEvt = do
  evt <- sumButton
  s <- holdDyn 0 evt
  el "div" (dynText $ fmap (T.pack . show) s)

-----------------------------------------------------------------------------------------------------------------

data Pagina = Principal | Barracas | Ofertas | ExemplosAula

clickLi :: DomBuilder t m => Pagina -> T.Text -> m (Event t Pagina)
clickLi p t = do
  (ev, _) <- elAttr' "li" ("class" =: "nav-item") (elAttr "a" ("href" =: "#" <> "class" =: "nav-link") (text t))
  return ((\_ -> p) <$> domEvent Click ev)

currPag :: (DomBuilder t m, PostBuild t m, MonadHold t m) => Pagina -> m ()
currPag p = 
  case p of 
    Principal -> principalPag
    Barracas -> barracasPag
    Ofertas -> ofertasPag
    ExemplosAula -> exemplosPag

principalPag :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m ()
principalPag = do
      pagina <- el "div" menu
      el "h1" $ text "Trabalho P2 - Haskell"
      el "p" $ text "Grupo: Douglas C. Pedra | Gabriel | Mariana | Thales"
      elAttr "img" ("src" =: static @"agricultor.jpg") blank

barracasPag :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m ()
barracasPag = do
  el "h3" (text "Pagina Barracas")
  el "span" (text "Pagina em construcao")

ofertasPag :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m ()
ofertasPag = do
  el "h3" (text "Pagina Ofertas")
  el "span" (text "Pagina em construcao")

exemplosPag :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m ()
exemplosPag = do
  el "h3" $ text "Exemplos da Aula:"
  el "span" $ text "listaAtr aplicado em menu :"
  menuEx
  el "h3" $ text "caixas 1 :"
  caixas
  el "h3" $ text "caixaSoma :"
  caixaSoma
  el "h3" $ text "revText :"
  bttnEvt
  el "h3" $ text "sumEvt :"
  sumEvt      
  

menu :: (DomBuilder t m, MonadHold t m) => m (Dynamic t Pagina)
menu = do
  evs <- elAttr "nav" ("class" =: "navbar navbar-expand-sm navbar-dark bg-dark") $ do
          elAttr "div" ("class" =: "container-fluid") $ do
            elAttr "a" ("class" =: "navbar-brand" <> "href" =: "#") (text "i-Feira")
            elAttr "button" ("class" =: "navbar-toggler"
                          <> "type" =: "button"
                          <> "data-bs-toggle" =: "collapse"
                          <> "data-bs-target" =: "#navbarNav"
                          <> "aria-controls" =: "navbarNav"
                          <> "aria-expanded" =: "false"
                          <> "aria-label" =: "Toggle navigation") $ do
              elAttr "span" ("class" =: "navbar-toggler-icon") blank
            elAttr "div" ("class" =: "collapse navbar-collapse" <> "id" =: "navbarNav") $ do
              elAttr "ul" ("class" =: "navbar-nav") $ do
                p1 <- clickLi Principal "Pagina Inicial"
                p2 <- clickLi Barracas "Barracas"
                p3 <- clickLi Ofertas "Ofertas"
                p4 <- clickLi ExemplosAula "Exemplos da Aula"
                return (leftmost [p1,p2,p3,p4])
  holdDyn Principal evs

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
      elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
  , _frontend_body = principalPag
  }
