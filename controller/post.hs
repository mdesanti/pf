{-# LANGUAGE OverloadedStrings, CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}

module Controller.Post where

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
import Model.Blog
import Model.Comment
import Acid


newForm :: AcidState Blog -> ServerPart Response
newForm acid = createForm (BlogPost (PostId 0) "" "") "/create_post" ""


editForm :: AcidState Blog -> Integer -> ServerPart Response
editForm acid key = 
                    do post <- query' acid (GetPost (PostId key))
                       case post of
                        Just (BlogPost a b c) -> createForm (BlogPost a b c) "/update_post" ""
                        Nothing -> badRequest (toResponse (("Could not find post with id " ++ show key) :: String))


handleNewForm :: AcidState Blog -> ServerPart Response
handleNewForm acid =
   do post_data <- getDataFn postRq
      case post_data of
        Left e -> badRequest (toResponse (Prelude.unlines e))
        Right(BlogPost post_id post_title post_content) 
                  | isValidBlog (BlogPost post_id post_title post_content) ->
                    do (BlogPost (PostId post_id) title content) <- update' acid (AddPost post_title post_content)
                       return (redirect 302 ("posts/" ++ show post_id) (toResponse ()))
                  | otherwise -> createForm (BlogPost (PostId 0) "" "") "/create_post" "Title or Content can not be empty"


handleEditForm :: AcidState Blog -> ServerPart Response
handleEditForm acid =
   do post_data <- getDataFn postRq
      case post_data of
        Left e -> badRequest (toResponse (Prelude.unlines e))
        Right(BlogPost (PostId post_id) post_title post_content)
                  | isValidBlog (BlogPost (PostId post_id) post_title post_content) -> 
                    do post <- update' acid (UpdatePost (BlogPost (PostId post_id) post_title post_content))
                       return (redirect 302 ("/posts/" ++ show post_id) (toResponse ()))
                  | otherwise -> createForm (BlogPost (PostId 0) "" "") "/create_post" "Title or Content can not be empty"


showPost :: AcidState Blog -> AcidState Comments -> Integer -> ServerPart Response
showPost acid comment_acid post_id =
            do post <- query' acid (GetPost (PostId post_id))
               case post of
                  Just post -> do
                                comments <- query' comment_acid (GetCommentsForPost (PostId post_id))
                                buildShowResponse post comments
                  Nothing -> badRequest (toResponse (("Could not find post with id " ++ show post_id) :: String))

handleDeletePost :: AcidState Blog -> Integer -> ServerPart Response
handleDeletePost acid post_id = 
            do to_delete <- query' acid (GetPost (PostId post_id))
               case to_delete of
                Just blog_post -> do 
                                    update' acid (DeletePost blog_post)
                                    return (redirect 302 ("/allPosts" :: String) (toResponse ()))
                Nothing -> badRequest (toResponse (("Could not find post with id " ++ show post_id) :: String))

handleAllPosts :: AcidState Blog -> ServerPart Response
handleAllPosts acid = 
  do posts <- query' acid AllPosts
     buildResponse posts



postRq :: RqData BlogPost
postRq = do
          title <- look "post_title"
          content <- look "post_content"
          postId <- lookRead "post_id"
          return (BlogPost (PostId postId) title content)