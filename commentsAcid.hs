{-# LANGUAGE OverloadedStrings, CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}

module CommentsAcid where

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
import Model.Comment
import Model.Blog

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





