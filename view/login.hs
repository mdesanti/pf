{-# LANGUAGE OverloadedStrings, CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}

module View.Login where

  import Control.Applicative  ((<$>), (<*>))
  import Control.Monad
  import Control.Monad.State
  import Control.Monad.Reader ( ask )
  import Control.Exception    ( bracket )
  import Happstack.Server(Method(GET, HEAD, POST, DELETE), dir, methodM, ServerPart, Response,
                          toResponse, simpleHTTP, nullConf, ok, toMessage, look, lookRead,
                          defaultBodyPolicy, BodyPolicy, decodeBody, RqData,
                          getDataFn, badRequest, lookFile, path, resp, seeOther, method,
                          getHeaderM, unauthorized, setHeaderM, askRq, getHeader, lookCookieValue,
                          CookieLife(Session), addCookie, mkCookie, HasRqData)
  import           Text.Blaze
  import           Text.Blaze.Internal
  import qualified Text.Blaze.Html4.Strict as H
  import qualified Text.Blaze.Html4.Strict.Attributes as A
  import System.IO
  import System.Log.Logger ( updateGlobalLogger
                           , rootLoggerName
                           , setLevel
                           , Priority(..)
                           )
  import Happstack.Server.Internal.Types
  import Data.ByteString
  import Data.ByteString.Base64 as Base64
  import qualified Data.Text as B
  import Data.ByteString.Char8 as C
  import Data.Text.Encoding
  import Data.Map as M
  import Data.Data            ( Data, Typeable )
  import Data.Acid            ( AcidState, Query, Update
                              , makeAcidic, openLocalState )
  import Data.Acid.Advanced   ( query', update' )
  import Data.Acid.Local      ( createCheckpointAndClose )
  import Data.SafeCopy        ( SafeCopy, base, deriveSafeCopy )
  import Data.IxSet           ( Indexable(..), IxSet(..), (@=)
                              , Proxy(..), getOne, ixFun, ixSet )
  import qualified Data.IxSet as IxSet
  import Happstack.Server.FileServe
  import System.Log.Logger
  import View.Posts
  import View.Template

  data AuthCredentials = AuthCredentials { username :: String,  password :: String }
  isValid :: AuthCredentials -> Bool
  isValid (AuthCredentials username password) = username == "admin" && password == "admin"

  authInfo :: RqData AuthCredentials
  authInfo = do
       username <- look "username"
       password <- look "password"
       return (AuthCredentials username password)

  createLoginForm :: String -> ServerPart Response
  createLoginForm error_message = ok (toResponse (appTemplate "Programación Funcional" [] $ do
        H.div (H.h1 "Login") H.! A.class_ "page-header"
        H.form H.! A.enctype "multipart/form-data" H.! A.class_ "form-horizontal" 
          H.! A.method "POST"
          H.! A.action (stringValue "/login") $ do
            H.p (H.toHtml error_message)
            H.div H.! A.class_ "control-group" $ do
              H.label "Username" H.! A.class_ "control-label"
              H.div H.! A.class_ "controls" $ do  
                H.input H.! A.type_ "text" H.! A.name "username"
            H.div H.! A.class_ "control-group" $ do
              H.label "Password" H.! A.class_ "control-label"
              H.div H.! A.class_ "controls" $ do  
                H.input H.! A.type_ "password" H.! A.name "password"
            H.div H.! A.class_ "control-group" $ do
              H.div H.! A.class_ "controls" $ do  
                H.input H.! A.type_ "submit" H.! A.value "Login" H.! A.class_ "btn btn-primary"))