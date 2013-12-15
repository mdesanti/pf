{-# LANGUAGE OverloadedStrings, CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}

module View.Posts where

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
  import Model.Blog
  import Model.Comment
  import View.Template
  import View.Comments

------------------------------------------ CREATE ONE POST --------------------------------------------
  createForm :: BlogPost -> String -> String -> ServerPart Response
  createForm (BlogPost (PostId key) title content) post_url error_message = ok $ toResponse $
      appTemplate "Programación Funcional" [] $ do
        H.div (H.h1 "Post Form") H.! A.class_ "page-header"
        H.form H.! A.enctype "multipart/form-data" H.! A.class_ "form-horizontal" 
          H.! A.method "POST"
          H.! A.action (stringValue post_url) $ do
            H.p (H.toHtml error_message)
            H.div H.! A.class_ "control-group" $ do
              H.label "Post Title" H.! A.class_ "control-label"
              H.div H.! A.class_ "controls" $ do  
                H.input H.! A.type_ "text" H.! A.name "post_title" H.! A.value (stringValue title)
            H.div H.! A.class_ "control-group" $ do
              H.label "Post Content" H.! A.class_ "control-label"
              H.div H.! A.class_ "controls" $ do  
                H.textarea H.! A.style "resize:none" 
                           H.! A.type_ "text" 
                           H.! A.name "post_content" 
                           H.! A.cols (H.toValue (60 ::Integer)) 
                           H.! A.rows (H.toValue (10 ::Integer)) $ (H.toHtml content)
            H.div H.! A.class_ "control-group" $ do
              H.div H.! A.class_ "controls" $ do  
                H.input H.! A.type_ "hidden" H.! A.name "post_id" H.! A.value (stringValue (show key))
                H.input H.! A.type_ "submit" H.! A.value "Upload" H.! A.class_ "btn btn-primary"
        H.a "Back" H.! A.href "/allPosts" H.! A.class_ "btn"

------------------------------------------ CREATE ONE POST --------------------------------------------

------------------------------------------ SHOW ONE POST ----------------------------------------------
  buildShowResponse :: BlogPost -> [Comment] -> ServerPart Response
  buildShowResponse (BlogPost key post_title post_content) comments = 
    ok (toResponse (
          appTemplate "Programación Funcional"
            []
            (do H.div ( H.h1 (H.toHtml ("Showing " ++ post_title))) H.! A.class_ "page-header"
                H.div H.! A.class_ "hero-unit" $ do
                  H.p (H.toHtml post_content)
                buildDeleteLink key
                H.div ( H.h1 ("Comments"))
                showComments comments
                createCommentForm (Comment (CommentId 0) "" key) "create_comment" ""
                H.a "Back" H.! A.href "/allPosts" H.! A.class_ "btn"
            )
      ))

  buildDeleteLink :: PostId -> H.Html
  buildDeleteLink (PostId key) = H.form
                                  H.! A.method "POST"
                                  H.! A.action (stringValue ("/posts/delete/" ++ show key)) $ do
                                      buildEditLink (PostId key)
                                      H.input H.! A.type_ "submit" H.! A.value "Delete" H.! A.class_ "btn btn-danger"

  buildEditLink :: PostId -> H.Html
  buildEditLink (PostId key) = H.a "Edit" H.! A.href (stringValue ("/update_post/" ++ (show key))) H.! A.class_ "btn btn-success"
  ------------------------------------------ SHOW ONE POST ----------------------------------------------

  ------------------------------------------ SHOW ALL POSTS ---------------------------------------------

  buildResponse :: [BlogPost] -> ServerPart Response
  buildResponse posts = 
    ok (toResponse (
          appTemplate "Programación Funcional" [] $ do
            H.div (H.h1 "Posts") H.! A.class_ "page-header"
            H.ul H.! A.class_ "unstyled" $ forM_ posts (H.li . (\(BlogPost key title content) -> H.a H.! (buildLink key) $ H.toHtml title)) H.! A.class_ "alert alert-success"
            H.a "Upload" H.! A.href "/upload" H.! A.class_ "btn btn-primary"
      ))

  buildLink :: PostId -> H.Attribute
  buildLink (PostId key) = A.href (stringValue ("/posts/" ++ (show key)))

  ------------------------------------------ SHOW ALL POSTS ---------------------------------------------