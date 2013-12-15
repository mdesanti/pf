{-# LANGUAGE OverloadedStrings, CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}

module Main where

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
                        CookieLife(Session), addCookie, mkCookie, HasRqData, askRqEnv, cookieValue)
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
import Data.Maybe
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
import View.Template
import Model.Blog
import Model.Comment
import Model.User
import Controller.Auth
import Controller.Post
import Controller.Comment
import Controller.User
import Acid

------------------------------------------ MAIN --------------------------------------------------------

-- simpleHTTP :: (ToMessage a) => Conf -> ServerPartT IO a -> IO ()

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" (10*10^6) 1000 1000)

main :: IO ()
main = 
  do updateGlobalLogger rootLoggerName (setLevel INFO)
     bracket (openLocalState initialUsersState)
             (createCheckpointAndClose)
             (\userAcid -> bracket (openLocalState initialCommentsState)
                                   (createCheckpointAndClose)
                                   (\commentAcid -> bracket (openLocalState initialBlogState)
                                                            (createCheckpointAndClose)
                                                            (\acid -> simpleHTTP nullConf (
                                                                do decodeBody myPolicy
                                                                   msum [ 
                                                                          dir "static" (serveDirectory DisableBrowsing [] "public"),
                                                                          dir "register" (do
                                                                                            method GET
                                                                                            register),
                                                                          dir "register" (do
                                                                                            method POST
                                                                                            handleRegister userAcid),
                                                                          dir "users" (do
                                                                                         method GET
                                                                                         listUsers userAcid),
                                                                          dir "login" (do
                                                                                        method GET
                                                                                        login),
                                                                          dir "login" (do
                                                                                          method POST
                                                                                          handleLogin userAcid),
                                                                          dir "upload" (do
                                                                                          myAuth userAcid
                                                                                          method GET 
                                                                                          newForm acid),
                                                                          dir "create_post" (do 
                                                                                                myAuth userAcid
                                                                                                method POST
                                                                                                handleNewForm acid),
                                                                          dir "posts" ( dir "create_comment" (do 
                                                                                                  method POST
                                                                                                  handleNewCommentForm commentAcid acid)),
                                                                          dir "update_post" (do 
                                                                                                myAuth userAcid
                                                                                                method POST
                                                                                                handleEditForm acid),
                                                                          dir "allPosts" (do method GET
                                                                                             handleAllPosts acid),
                                                                          dir "posts" ( dir "delete" ( path ( (\s -> do 
                                                                                                                        myAuth userAcid
                                                                                                                        method POST
                                                                                                                        handleDeletePost acid s)))),
                                                                          dir "posts" ( do path ( (\s -> do method GET
                                                                                                            showPost acid commentAcid s))),
                                                                          dir "update_post" ( do path ( (\s -> do 
                                                                                                                  myAuth userAcid
                                                                                                                  method GET
                                                                                                                  editForm acid s))),
                                                                          seeOther ("/allPosts" :: String) (toResponse ())
                                                                        ]))))
------------------------------------------ MAIN --------------------------------------------------------
myAuth :: AcidState Users -> ServerPart ()
myAuth acid = do
          userCookie <- (lookCookieValue "User")
          passwordCookie <- (lookCookieValue "Password")
          exists <- query' acid (UserExists userCookie passwordCookie)
          guard (exists)


initialBlogState :: Blog
initialBlogState =
    Blog { nextPostId = PostId 1
         , posts      = IxSet.empty
         }

initialCommentsState :: Comments
initialCommentsState =
    Comments { nextCommentId = CommentId 1
              , comments      = IxSet.empty
             }

initialUsersState :: Users
initialUsersState = 
    Users { nextUserId = UserId 1,
            users = IxSet.empty }

