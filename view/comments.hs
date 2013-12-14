{-# LANGUAGE OverloadedStrings, CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}

module View.Comments where

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
  import Model.Comment
  import Model.Blog
  import View.Template

------------------------------------------ CREATE ONE COMMENT --------------------------------------------
  createCommentForm :: Comment -> String -> String -> H.Html
  createCommentForm (Comment (CommentId key) comment_content (PostId post_key)) post_url error_message =
    H.form H.! A.enctype "multipart/form-data"
      H.! A.method "POST"
      H.! A.action (stringValue post_url) $ do
        H.p (H.toHtml error_message)
        H.div H.! A.class_ "control-group" $ do
          H.div H.! A.class_ "controls" $ do  
            H.textarea H.! A.style "resize:none" H.! A.type_ "text" H.! A.name "comment_content" H.! A.cols (H.toValue (120 ::Integer)) H.! A.rows (H.toValue (10 ::Integer)) $ (H.toHtml comment_content)
        H.div H.! A.class_ "control-group" $ do
          H.div H.! A.class_ "controls" $ do  
            H.input H.! A.type_ "hidden" H.! A.name "post_id" H.! A.value (stringValue (show post_key))
            H.input H.! A.type_ "hidden" H.! A.name "comment_id" H.! A.value (stringValue (show key))
            H.input H.! A.type_ "submit" H.! A.value "Upload" H.! A.class_ "btn btn-primary"

------------------------------------------ CREATE ONE COMMENT --------------------------------------------

--------------------------------------------- SHOW ALL COMMENTS  -----------------------------------------
  showComments :: [Comment] -> H.Html
  showComments comments = 
    H.ul H.! A.class_ "unstyled" $ forM_ comments (H.li . (\(Comment key content post_key) -> H.toHtml content)) H.! A.class_ "alert alert-info"

--------------------------------------------- SHOW ALL COMMENTS ---------------------------------------------