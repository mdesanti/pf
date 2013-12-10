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



------------------------------------------ POST DEFINITION ---------------------------------------------

newtype PostId = PostId { unPostId :: Integer } deriving (Eq, Ord, Show, Read, Data, Enum, Typeable)

data BlogPost = BlogPost { postId :: PostId, title :: String, content :: String } deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Indexable BlogPost where
  empty = ixSet
    [ ixFun $ \bp -> [ postId bp ]]

data Blog = Blog
    { nextPostId :: PostId
    , posts      :: IxSet BlogPost
    }
    deriving (Data, Typeable)

getName (BlogPost _ title content) = title
getContent (BlogPost _ title content) = content
getId (BlogPost key _ _) = key

------------------------------------------ POST DEFINITION ---------------------------------------------
------------------------------------------ ACID CONFIGURATION-------------------------------------------
$(deriveSafeCopy 0 'base ''PostId)
$(deriveSafeCopy 0 'base ''BlogPost)
$(deriveSafeCopy 0 'base ''Blog)

initialBlogState :: Blog
initialBlogState =
    Blog { nextPostId = PostId 1
         , posts      = IxSet.empty
         }

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

------------------------------------------ ACID CONFIGURATION ------------------------------------------
------------------------------------------ MAIN --------------------------------------------------------

-- simpleHTTP :: (ToMessage a) => Conf -> ServerPartT IO a -> IO ()

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" (10*10^6) 1000 1000)

main :: IO ()
main = 
  do updateGlobalLogger rootLoggerName (setLevel INFO)
     bracket (openLocalState initialBlogState)
             (createCheckpointAndClose)
              (\acid -> simpleHTTP nullConf (
                          do decodeBody myPolicy
                             msum [ 
                                    dir "static" (serveDirectory DisableBrowsing [] "public"), 
                                    dir "login" (do
                                                  method GET
                                                  createLoginForm),
                                    dir "login" (do
                                                    method POST
                                                    handleLogin),
                                    dir "upload" (do
                                                    myAuth
                                                    methodM [GET, HEAD] 
                                                    newForm acid),
                                    dir "create_post" (do 
                                                          myAuth
                                                          method POST
                                                          handleNewForm acid),
                                    dir "update_post" (do 
                                                          myAuth
                                                          method POST
                                                          handleEditForm acid),
                                    dir "allPosts" (do method GET
                                                       handleAllPosts acid),
                                    dir "posts" ( dir "delete" ( path ( (\s -> do 
                                                                                  myAuth
                                                                                  method POST
                                                                                  handleDeletePost acid s)))),
                                    dir "posts" ( do path ( (\s -> do method GET
                                                                      showPost acid s))),
                                    dir "update_post" ( do path ( (\s -> do 
                                                                            myAuth
                                                                            method POST
                                                                            editForm acid s))),
                                    seeOther ("/allPosts" :: String) (toResponse ())
                                  ]))
------------------------------------------ MAIN --------------------------------------------------------
--setUserCookie =  do
--           addCookie Session (mkCookie "User" "pepe")
--           return ()
myAuth = do
          userCookie <- (lookCookieValue "User")
          liftIO $ print (show userCookie)
          return ()
------------------------------------------ COMMON POST FORM --------------------------------------------
postRq :: RqData (String, String, Integer)
postRq =
    (,,) <$> look "post_title" <*> look "post_content" <*> lookRead "post_id"


createForm :: AcidState Blog -> BlogPost -> String -> ServerPart Response
createForm acid (BlogPost (PostId key) title content) post_url = ok $ toResponse $
    appTemplate "Programación Funcional" [] $ do
      H.div (H.h1 "New Post") H.! A.class_ "page-header"
      H.form H.! A.enctype "multipart/form-data" H.! A.class_ "form-horizontal" 
        H.! A.method "POST"
        H.! A.action (stringValue post_url) $ do
          H.div H.! A.class_ "control-group" $ do
            H.label "Post Title" H.! A.class_ "control-label"
            H.div H.! A.class_ "controls" $ do  
              H.input H.! A.type_ "text" H.! A.name "post_title" H.! A.value (stringValue title)
          H.div H.! A.class_ "control-group" $ do
            H.label "Post Content" H.! A.class_ "control-label"
            H.div H.! A.class_ "controls" $ do  
              H.textarea H.! A.type_ "text" H.! A.name "post_content" H.! A.cols (H.toValue (60 ::Integer)) H.! A.rows (H.toValue (10 ::Integer)) $ (H.toHtml content)
          H.div H.! A.class_ "control-group" $ do
            H.div H.! A.class_ "controls" $ do  
              H.input H.! A.type_ "hidden" H.! A.name "post_id" H.! A.value (stringValue (show key))
              H.input H.! A.type_ "submit" H.! A.value "Upload" H.! A.class_ "btn btn-primary"
------------------------------------------ COMMON POST FORM --------------------------------------------

------------------------------------------ LOGIN FORM --------------------------------------------------
userInfo :: RqData (String, String)
userInfo =
    (,) <$> look "username" <*> look "password"

createLoginForm :: ServerPart Response
createLoginForm = ok (toResponse (appTemplate "Programación Funcional" [] $ do
      H.div (H.h1 "Login") H.! A.class_ "page-header"
      H.form H.! A.enctype "multipart/form-data" H.! A.class_ "form-horizontal" 
        H.! A.method "POST"
        H.! A.action (stringValue "/login") $ do
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
              H.input H.! A.type_ "submit" H.! A.value "Upload" H.! A.class_ "btn btn-primary"))

handleLogin :: ServerPart Response
handleLogin = do
                info <- getDataFn userInfo
                case info of
                  Left e -> badRequest (toResponse (Prelude.unlines e))
                  Right (username, password) -> do
                                                  addCookie Session (mkCookie "User" "pepe")
                                                  seeOther (show "/allPosts") (toResponse ())
------------------------------------------ LOGIN FORM --------------------------------------------------


------------------------------------------ POST UPLOAD -------------------------------------------------

newForm :: AcidState Blog -> ServerPart Response
newForm acid = createForm acid (BlogPost (PostId 0) "" "") "/create_post"

handleNewForm :: AcidState Blog -> ServerPart Response
handleNewForm acid =
   do post_data <- getDataFn postRq
      case post_data of
        Left e -> badRequest (toResponse (Prelude.unlines e))
        Right(post_title, post_content, post_id) -> 
                    do (BlogPost (PostId post_id) title content) <- update' acid (AddPost post_title post_content)
                       seeOther ("/posts/" ++ show post_id) (toResponse ())
------------------------------------------ POST UPLOAD -------------------------------------------------

------------------------------------------ POST UPDATE -------------------------------------------------


editForm :: AcidState Blog -> Integer -> ServerPart Response
editForm acid key = 
                    do post <- query' acid (GetPost (PostId key))
                       case post of
                        Just (BlogPost a b c) -> createForm acid (BlogPost a b c) "/update_post"
                        Nothing -> badRequest (toResponse (("Could not find post with id " ++ show key) :: String))

handleEditForm :: AcidState Blog -> ServerPart Response
handleEditForm acid =
   do post_data <- getDataFn postRq
      case post_data of
        Left e -> badRequest (toResponse (Prelude.unlines e))
        Right(post_title, post_content, post_id) -> 
                    do post <- update' acid (UpdatePost (BlogPost (PostId post_id) post_title post_content))
                       seeOther ("/posts/" ++ show post_id) (toResponse ())
------------------------------------------ POST UPDATE -------------------------------------------------

-------------------------------------------- TEMPLATE --------------------------------------------------
appTemplate :: String -> [H.Html] -> H.Html -> H.Html
appTemplate title headers body =
    H.html $ do
      H.link H.! A.rel "stylesheet" H.! A.type_ "text/css" H.! A.href "/static/css/bootstrap.css"
      H.head $ do
        H.title (H.toHtml title)
        H.meta H.! A.httpEquiv "Content-Type"
               H.! A.content "text/html;charset=utf-8"
        sequence_ headers
      H.body $ do
        H.div H.! A.class_ "container" $ do
          body
-------------------------------------------- TEMPLATE --------------------------------------------------

------------------------------------------ SHOW ONE POST ----------------------------------------------

showPost :: AcidState Blog -> Integer -> ServerPart Response
showPost acid post_id =
            do post <- query' acid (GetPost (PostId post_id))
               case post of
                  Just post -> buildShowResponse post
                  Nothing -> badRequest (toResponse (("Could not find post with id " ++ show post_id) :: String))

buildShowResponse :: BlogPost -> ServerPart Response
buildShowResponse (BlogPost key post_title post_content) = 
  ok (toResponse (
        appTemplate "Programación Funcional"
          []
          (do H.div ( H.h1 (H.toHtml ("Showing " ++ post_title))) H.! A.class_ "page-header"
              H.div H.! A.class_ "hero-unit" $ do
                H.p (H.toHtml post_content)
              buildDeleteLink key
          )
    ))

buildDeleteLink :: PostId -> H.Html
buildDeleteLink (PostId key) = H.form
                                H.! A.method "POST"
                                H.! A.action (stringValue ("/posts/delete/" ++ show key)) $ do
                                   H.input H.! A.type_ "submit" H.! A.value "Delete" H.! A.class_ "btn btn-primary"

------------------------------------------ SHOW ONE POST ----------------------------------------------

------------------------------------------ DELETE POST ------------------------------------------------

handleDeletePost :: AcidState Blog -> Integer -> ServerPart Response
handleDeletePost acid post_id = 
            do to_delete <- query' acid (GetPost (PostId post_id))
               case to_delete of
                Just blog_post -> do 
                                    update' acid (DeletePost blog_post)
                                    seeOther ("/allPosts" :: String) (toResponse ())
                Nothing -> badRequest (toResponse (("Could not find post with id " ++ show post_id) :: String))

------------------------------------------ DELETE POST ------------------------------------------------

------------------------------------------ SHOW ALL POSTS ----------------------------------------------

handleAllPosts :: AcidState Blog -> ServerPart Response
handleAllPosts acid = 
  do posts <- query' acid AllPosts
     buildResponse posts

buildResponse :: [BlogPost] -> ServerPart Response
buildResponse posts = 
  ok (toResponse (
        appTemplate "Programación Funcional" [] $ do
          H.div (H.h1 "Posts") H.! A.class_ "page-header"
          H.ul $ forM_ posts (H.li . (\(BlogPost key title content) -> H.a H.! (buildLink key) $ H.toHtml title))
    ))

buildLink :: PostId -> H.Attribute
buildLink (PostId key) = A.href (stringValue ("/posts/" ++ (show key)))

------------------------------------------ SHOW ALL POSTS ----------------------------------------------


------------------------------------------ HOME --------------------------------------------------------
home :: AcidState Blog -> ServerPart Response
home acid = 
      do posts <- query' acid AllPosts
         ok $ toResponse $
            appTemplate "Programación Funcional"
              [H.meta H.! A.name "keywords"
                      H.! A.content "happstack, blaze, html"
              ]
              (H.p $ H.toHtml ("A ver... " ++ show (Prelude.length posts)))
------------------------------------------ HOME --------------------------------------------------------



