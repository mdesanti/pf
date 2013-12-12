{-# LANGUAGE OverloadedStrings, CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}

module Acid where

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
import Model.Blog
import Model.Comment

$(deriveSafeCopy 0 'base ''PostId)
$(deriveSafeCopy 0 'base ''BlogPost)
$(deriveSafeCopy 0 'base ''Blog)

addPost :: String -> String -> Update Blog BlogPost
addPost post_title post_content =
    do b@Blog{..} <- get
       let post = BlogPost { postId = nextPostId
                             , title  = post_title
                             , content = post_content
                           }
       put $ b { nextPostId = succ nextPostId
               , posts      = IxSet.insert post posts
               }
       return post

getPost :: PostId -> Query Blog (Maybe BlogPost)
getPost key = 
    do Blog{..} <- ask
       return (getOne (posts @= key))


allPosts :: Query Blog [BlogPost]
allPosts = do
             Blog{..} <- ask
             let all_posts = IxSet.toList posts
             return all_posts

updatePost :: BlogPost -> Update Blog ()
updatePost (BlogPost key title content) = do
  b@Blog{..} <- get
  put $ b { posts =
             IxSet.updateIx key (BlogPost key title content) posts
          }
deletePost :: BlogPost -> Update Blog ()
deletePost post = do
  b@Blog{..} <- get
  put $ b { posts = IxSet.delete post posts}

$(makeAcidic ''Blog ['addPost, 'allPosts, 'getPost, 'updatePost, 'deletePost])




$(deriveSafeCopy 0 'base ''CommentId)
$(deriveSafeCopy 0 'base ''Comment)
$(deriveSafeCopy 0 'base ''Comments)

addComment :: String -> PostId -> Update Comments Comment
addComment comment_content post_postId =
    do b@Comments{..} <- get
       let comment = Comment { commentId = nextCommentId
                             , content  = comment_content
                             , postId = post_postId
                           }
       put $ b { nextCommentId = succ nextCommentId
               , comments      = IxSet.insert comment comments
               }
       return comment

getCommentsForPost :: PostId -> Query Comments [Comment]
getCommentsForPost postId = 
    do Comments{..} <- ask
       let all_comments = IxSet.toList comments
       return (Prelude.filter (\c -> (getPostId c) == postId) all_comments)


$(makeAcidic ''Comments ['addComment, 'getCommentsForPost])




