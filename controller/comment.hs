{-# LANGUAGE OverloadedStrings, CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}

module Controller.Comment where

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
import View.Comments
import View.Posts
import Model.Comment
import Model.Blog
import Acid

handleNewCommentForm :: AcidState Comments -> AcidState Blog -> ServerPart Response
handleNewCommentForm acid post_acid =
   do
      post_data <- getDataFn commentRq
      case post_data of
        Left e -> badRequest (toResponse (Prelude.unlines e))
        Right(Comment comment_id comment_content post_id) 
                  | isValidComment (Comment comment_id comment_content post_id)  ->
                    do (Comment (CommentId comment_id) comment_content (PostId post_id)) <- update' acid (AddComment comment_content post_id)
                       return (redirect 302 ("/posts/" ++ show post_id) (toResponse ()))
                  | otherwise -> do post <- query' post_acid (GetPost post_id)
                                    case post of
                                      Just (BlogPost a b c) -> do
                                                                comments <- query' acid (GetCommentsForPost post_id)
                                                                buildShowResponse (BlogPost a b c) comments
                                      Nothing -> badRequest (toResponse (("Could not find post with id " ++ show post_id) :: String))

commentRq :: RqData Comment
commentRq = do
          commentId <- lookRead "comment_id"
          content <- look "comment_content"
          postId <- lookRead "post_id"
          return (Comment (CommentId commentId) content (PostId postId))
