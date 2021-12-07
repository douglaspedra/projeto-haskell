{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}

module Frontend where

import Control.Monad
import Control.Monad.Fix
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
import Data.Aeson


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.



-----------------------------------------------------------------------------------------------------------------

data Pagina = Principal | Barracas | Ofertas | Produtos | Funcionarios | Cadastros | ExemplosAula

data Acao = Perfil Int | Editar Int | Deletar Int

clickLi :: DomBuilder t m => Pagina -> T.Text -> m (Event t Pagina)
clickLi p t = do
  (ev, _) <- elAttr' "li" ("class" =: "nav-item") (elAttr "a" ("href" =: "#" <> "class" =: "nav-link") (text t))
  return ((\_ -> p) <$> domEvent Click ev)

currPag :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, Prerender js t m) => Pagina -> m ()
currPag p = 
  case p of 
    Principal -> principalPag
    Barracas -> reqListaBarraca
    Ofertas -> ofertasPag
    Produtos -> produtosPag
    Funcionarios -> funcionariosPag
    Cadastros -> reqBarraca
    ExemplosAula -> exemplosPag

mainPag :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, Prerender js t m) => m ()
mainPag = do
  pagina <- el "div" menu
  dyn_ $ currPag <$> pagina
  

menu :: (DomBuilder t m, MonadHold t m, MonadFix m, Prerender js t m) => m (Dynamic t Pagina)
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
                p4 <- clickLi Funcionarios "Funcionarios"
                p5 <- clickLi Cadastros "Cadastros"
                p6 <- clickLi ExemplosAula "Exemplos da Aula"
                return (leftmost [p1,p2,p3,p4,p5,p6])
  holdDyn Principal evs 


principalPag :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m ()
principalPag = do
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

funcionariosPag :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m ()
funcionariosPag = do
  el "h3" (text "Pagina Funcionarios")
  el "span" (text "Pagina em construcao")

produtosPag :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m ()
produtosPag = do
  el "h3" (text "Pagina Produtos")
  el "span" (text "Pagina em construcao")

exemplosPag :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
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
  el "h3" $ text "Contador :"
  pagClick 

--para obter o /cliente /barraca /buscar/13
getPath :: R BackendRoute -> T.Text
getPath p = renderBackendRoute checFullREnc p

sendRequest :: ToJSON a => R BackendRoute -> a -> XhrRequest T.Text
sendRequest r dados = postJson (getPath r) dados


getBarracaListReq :: XhrRequest ()
getBarracaListReq = xhrRequest "GET" (getPath (BackendRoute_BarracaListar :/ ())) def

getOfertaListReq :: XhrRequest ()
getOfertaListReq = xhrRequest "GET" (getPath (BackendRoute_OfertaListar :/ ())) def

getProdutoListReq :: XhrRequest ()
getProdutoListReq = xhrRequest "GET" (getPath (BackendRoute_ProdutoListar :/ ())) def

getFuncionarioListReq :: XhrRequest ()
getFuncionarioListReq = xhrRequest "GET" (getPath (BackendRoute_FuncionarioListar :/ ())) def

getProdutoReq :: Int -> XhrRequest ()
getProdutoReq pid = xhrRequest "GET" (getPath (BackendRoute_ProdutoBuscar :/ pid)) def



req :: ( DomBuilder t m, Prerender js t m) => m ()
req = do
  el "h3" $ text "Insercao ao Banco de Dados:"
  inputEl <- inputElement def
  (submitBtn,_) <- el' "button" (text "Inserir")
  let click = domEvent Click submitBtn
  let nm = tag (current $ _inputElement_value inputEl) click
  _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_Cliente :/ ()) <$> nm))
  return ()


reqBarraca :: (DomBuilder t m, Prerender js t m) => m ()
reqBarraca = do
  el "h3" $ text "Insere informacaoes da Barraca:"
  el "label" (text "Nome: ")
  nome <- inputElement def
  el "label" (text "Categoria: ")
  categoria <- inputElement def
  let barraca = fmap (\(n,c) -> Barraca 0 n c) (zipDyn (_inputElement_value nome)(_inputElement_value categoria))
  (submitBtn,_) <- el' "button" (text "Inserir")
  let click = domEvent Click submitBtn
  let barrEvt = tag (current barraca) click
  _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_Barraca :/ ()) <$> barrEvt))
  return ()

reqProduto :: (DomBuilder t m, Prerender js t m) => m ()
reqProduto = do
  el "h3" $ text "Insere informacoes do produto:"
  nome <- inputElement def
  categoria <- inputElement def
  let barraca = fmap (\(n,c) -> Barraca 0 n c) (zipDyn (_inputElement_value nome)(_inputElement_value categoria))
  (submitBtn,_) <- el' "button" (text "Inserir")
  let click = domEvent Click submitBtn
  let barrEvt = tag (current barraca) click
  _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_Barraca :/ ()) <$> barrEvt))
  return ()




tabBarraca :: (PostBuild t m, DomBuilder t m) => Dynamic t Barraca -> m (Event t Acao)
tabBarraca br = do
  el "tr" $ do
    el "td" (dynText $ fmap (T.pack . show . barracaId) br)
    el "td" (dynText $ fmap barracaNome br)
    el "td" (dynText $ fmap barracaCategoria br)
    evt <- fmap (fmap (const Perfil)) (button "perfil")
    return (attachPromptlyDynWith (flip ($)) (fmap barracaId br) evt)

tabProduto :: (PostBuild t m, DomBuilder t m) => Dynamic t Produto -> m (Event t Acao)
tabProduto pr = do
  el "tr" $ do
    el "td" (dynText $ fmap (T.pack . show . idProduto) pr)
    el "td" (dynText $ fmap (T.pack . show .nomeProduto) pr)
    el "td" (dynText $ fmap (T.pack . show .categoriaProduto) pr)
    el "td" (dynText $ fmap (T.pack . show .valorProduto) pr)
    el "td" (dynText $ fmap (T.pack . show .qtdProduto) pr)
    evt1 <- fmap (fmap (const Editar)) (button "editar")
    evt2 <- fmap (fmap (const Deletar)) (button "deletar")
    return (attachPromptlyDynWith (flip ($)) (fmap idProduto pr) (leftmost [evt1,evt2]))


--reqTabela

reqBarracaLista :: (DomBuilder t m , Prerender js t m , MonadHold t m , MonadFix m , PostBuild t m) => Workflow t m T.Text
reqBarracaLista = Workflow $ do
  btn <- button "Listar Barracas"
  barracs :: Dynamic t (Event t (Maybe [Barraca])) <- prerender
    (pure never)
    (fmap decodeXhrResponse <$> performRequestAsync (const getBarracaListReq <$> btn))
  evt <- return (fmap (fromMaybe []) $ switchDyn barracs)
  dynB <- foldDyn (++) [] evt
  tb <- elAttr "table" ("class" =: "table table-striped") $ do
    el "thead" $ do
      el "tr" $ do
        el "th" (text "Id")
        el "th" (text "Nome")
        el "th" (text "Categoria")
    el "tbody" $ do
      simpleList dynB tabBarraca
  tb' <- return $ switchDyn $ fmap leftmost tb
  return ("", escolherPag <$> tb')
  where
    escolherPag (Perfil bid) = pagPerfilBarraca bid



pagPerfilBarraca :: (DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => Int -> Workflow t m T.Text
pagPerfilBarraca bid = Workflow $ do
  btn <- button "Exibir Produtos"
  prods :: Dynamic t (Event t (Maybe [Produto])) <- prerender
    (pure never)
    (fmap decodeXhrResponse <$> performRequestAsync (const getProdutoListReq <$> btn))
  evt <- return (fmap (fromMaybe []) $ switchDyn prods)
  dynB <- foldDyn (++) [] evt
  tb <- elAttr "table" ("class" =: "table table-striped") $ do
    el "thead" $ do
      el "tr" $ do
        el "th" (text "Id")
        el "th" (text "Nome")
        el "th" (text "Categoria")
        el "th" (text "Valor")
        el "th" (text "Qtd")
    el "tbody" $ do
      simpleList dynB tabProduto
  tb' <- return $ switchDyn $ fmap leftmost tb
  return ("", escolherPag <$> tb')
  where
    escolherPag (Editar pid) = pagPerfilProduto pid
  
pagPerfilProduto :: ( DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => Int -> Workflow t m T.Text
pagPerfilProduto pid = Workflow $ do
  btn <- button "mostrar"
  prod :: Dynamic t (Event t (Maybe Produto)) <- prerender
    (pure never)
    (fmap decodeXhrResponse <$> performRequestAsync (const (getProdutoReq pid) <$> btn))
  mdyn <- return (switchDyn prod)
  dynP <- return ((fromMaybe (Produto 0 0 "" "" 0 0)) <$> mdyn)
  el "div" $ do
    el "label" (text "Nome")
    nome <- inputElement $ def & inputElementConfig_setValue .~ (fmap nomeProduto dynP)
    el "label" (text "Categoria")
    categoria <- inputElement $ def & inputElementConfig_setValue .~ (fmap categoriaProduto dynP)
    el "label" (text "Valor")
    vl <- numberInputDyn (fmap valorProduto dynP)
    el "label" (text "Qtd")
    qt <- numberInputDyn (fmap qtdProduto dynP)

    let produ = fmap (\((n,c),(v,q)) -> Produto 0 0 n c v q) (zipDyn (zipDyn (_inputElement_value nome) (_inputElement_value categoria)) (zipDyn (vl)(qt)))
    submitBtn <- button "Editar"
    let prodEvt = tag (current produ) submitBtn
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_ProdutoEditar :/ pid) <$> prodEvt))
  --ret <- button "editar"
    return ("Editar: " <> (T.pack $ show pid), reqBarracaLista <$ submitBtn)




reqListaBarraca :: (DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
reqListaBarraca = do
  r <- workflow reqBarracaLista
  el "div" (dynText r)



numberInputDyn :: (DomBuilder t m, Num a, Read a, Show a) => Event t a -> m (Dynamic t a)
numberInputDyn p = do
    val <- return (fmap (T.pack . show) p)
    n <- inputElement $ def
      & inputElementConfig_setValue .~ val
    return $ fmap (fromMaybe 0 . readMaybe . T.unpack)
                  (_inputElement_value n)

-- tabBarraca :: DomBuilder t m => Barraca -> m ()
-- tabBarraca br = do
--   el "tr" $ do
--     el "td" (text $ T.pack $ show $ barracaId br)
--     el "td" (text $ barracaNome br)
--     el "td" (text $ barracaCategoria br)


-- reqBarracaLista :: ( DomBuilder t m , Prerender js t m , MonadHold t m , MonadFix m , PostBuild t m) => m ()
-- reqBarracaLista = do
--   (btn, _) <- el' "button" (text "Listar Barracas")
--   let click = domEvent Click btn
--   barracs :: Dynamic t (Event t (Maybe [Barraca])) <- prerender
--     (pure never)
--     (fmap decodeXhrResponse <$> performRequestAsync (const getBarracaListReq <$> click))
--   dynB <- foldDyn (\bs d -> case bs of
--                           Nothing -> []
--                           Just b -> d++b) [] (switchDyn barracs)
--   el "table" $ do
--     el "thead" $ do
--       el "tr" $ do
--         el "th" (text "Id")
--         el "th" (text "Nome")
--         el "th" (text "Categoria")
--     el "tbody" $ do
--       dyn_ (fmap sequence (ffor dynB (fmap tabBarraca)))

-- reqOferta :: (DomBuilder t m, Prerender js t m) => m ()
-- reqOferta = do
--   cdBarracaOferta <- numberInput
--   cdProdutoOferta <- numberInput
--   desconto <- numberInput
--   valorOferta <- numberInput
--   let oferta = fmap (\(n,c) -> Oferta 0 n c) (zipDyn (_inputElement_value nome)(_inputElement_value categoria))
--   (submitBtn,_) <- el' "button" (text "Inserir")
--   let click = domEvent Click submitBtn
--   let barrEvt = tag (current barraca) click
--   _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
--       (pure never)
--       (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_Barraca :/ ()) <$> barrEvt))
--   return ()

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

numberInput :: (DomBuilder t m, Num a, Read a) => m (Dynamic t a)
numberInput = do 
  n <- inputElement $ def
    & inputElementConfig_initialValue .~ "0"
    & inputElementConfig_elementConfig
    . elementConfig_initialAttributes .~ ("type" =: "number")
  return $ fmap (fromMaybe 0 . readMaybe . T.unpack)
              (_inputElement_value n)

caixaSoma :: (DomBuilder t m, PostBuild t m) => m ()
caixaSoma = do
  n1 <- numberInput -- Num a => m (Dynamic t Double)
  text " "
  n2 <- numberInput -- Num a => m (Dynamic t Double)
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

countClick :: DomBuilder t m => m (Event t Int)
countClick = do
  (ev, _) <- el' "button" (text "+")
  return ((const 1) <$> domEvent Click ev)

pagClick :: (MonadHold t m, PostBuild t m, DomBuilder t m, MonadFix m) => m ()
pagClick = do 
  evt <- countClick
  st <- accumDyn (+) 0 evt
  el "div" (dynText (fmap (T.pack . show) st))




frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "i-Feira"
      elAttr "link" ("href" =: static @"main.css" 
                  <> "type" =: "text/css" 
                  <> "rel" =: "stylesheet") blank

      elAttr "link" ("href" =: "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" 
                  <> "rel" =: "stylesheet" 
                  <> "integrity" =: "sha384-1BmE4kWBq78iYhFldvKuhfTAU6auU8tT94WrHftjDbrCEXSU1oBoqyl2QvZ6jIW3" 
                  <> "crossorigin" =: "anonymous") blank

      elAttr "script" ("href" =: "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js" 
                  <> "integrity" =: "sha384-ka7Sk0Gln4gmtz2MlQnikT1wXgYsOg+OMhuP+IlRH9sENBO0LRn5q+8nbTov4+1p" 
                  <> "crossorigin" =:"anonymous") blank

      elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
  , _frontend_body = mainPag
  }
