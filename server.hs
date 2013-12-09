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
                        unauthorized, getHeaderM, setHeaderM)
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
import Data.ByteString.Base64 as Base64
import Data.Map as M
import qualified Data.Text as B
import Data.Data            ( Data, Typeable )
import Data.Acid            ( AcidState, Query, Update
                            , makeAcidic, openLocalState )
import Data.Acid.Advanced   ( query', update' )
import Data.Acid.Local      ( createCheckpointAndClose )
import Data.SafeCopy        ( SafeCopy, base, deriveSafeCopy )
import Data.IxSet           ( Indexable(..), IxSet(..), (@=)
                            , Proxy(..), getOne, ixFun, ixSet )
import qualified Data.IxSet as IxSet

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

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" (10*10^6) 1000 1000)

-- Para entender como funciona el direccionamiento de rutas hay que entender
-- como funciona la funcion guard. methodM es un guard que chequea que el método
-- del request sea alguno de los pasados en el array. En caso de que sea así,
-- retora un valor monádico que represente "success" y entonces cuando se haga
-- bind con dicho valor se evalua si se ejecuta el comando siguiente (el handler)
main :: IO ()
main = 
  do updateGlobalLogger rootLoggerName (setLevel INFO)
     bracket (openLocalState initialBlogState)
             (createCheckpointAndClose)
              (\acid -> simpleHTTP nullConf $
                          do decodeBody myPolicy
                             msum [ do myAuth
                                       dir "upload" $ do method GET
                                       newForm acid,
                                    do dir "create_post" $ do method POST
                                       handleNewForm acid,
                                    do dir "update_post" $ do method POST
                                       handleEditForm acid,
                                    do dir "allPosts" $ do method GET
                                       handleAllPosts acid,
                                    do dir "posts" $ do dir "delete" $ do path $ (\s -> do method POST
                                                                                           handleDeletePost acid s),
                                    do dir "posts" $ do path $ (\s -> do
                                                                        method GET
                                                                        showPost acid s),
                                    do dir "update_post" $ do path $ (\s -> do method POST
                                                                               editForm acid s),
                                    seeOther ("/allPosts" :: String) (toResponse ())
                                  ])
------------------------------------------ MAIN --------------------------------------------------------
myAuth = basicAuth' "Test" (M.fromList [("hello", "world")]) (return "Login Failed")

basicAuth' realmName authMap unauthorizedPart = do
        let validLogin name pass = M.lookup name authMap == Just pass
        let parseHeader = Prelude.break (':'==) . Base64.decode . B.unpack . B.drop 6
        authHeader <- getHeaderM "authorization"
        case authHeader of
            Nothing -> err
            Just x  -> case parseHeader x of
                  (name, ':':pass) | validLogin name pass -> mzero
                                   | otherwise -> err
                _                                       -> err
    where
        err = do
            unauthorized ()
            setHeaderM headerName headerValue
            unauthorizedPart
        headerValue = "Basic realm=\"" ++ realmName ++ "\""
        headerName  = "WWW-Authenticate"

------------------------------------------ COMMON POST FORM --------------------------------------------
postRq :: RqData (String, String, Integer)
postRq =
    (,,) <$> look "post_title" <*> look "post_content" <*> lookRead "post_id"


createForm :: AcidState Blog -> BlogPost -> String -> ServerPart Response
createForm acid (BlogPost (PostId key) title content) post_url = ok $ toResponse $
    appTemplate "Programación Funcional" []
      (H.form H.! A.enctype "multipart/form-data"
            H.! A.method "POST"
            H.! A.action (stringValue post_url) $ do
               H.label "Post Title"
               H.input H.! A.type_ "text" H.! A.name "post_title" H.! A.value (stringValue title)
               H.label "Post Content"
               H.textarea H.! A.type_ "text" H.! A.name "post_content" H.! A.cols (H.toValue (60 ::Integer)) H.! A.rows (H.toValue (10 ::Integer)) $ (H.toHtml content)
               H.input H.! A.type_ "hidden" H.! A.name "post_id" H.! A.value (stringValue (show key))
               H.input H.! A.type_ "submit" H.! A.value "upload"
      )
------------------------------------------ COMMON POST FORM --------------------------------------------

------------------------------------------ POST UPLOAD -------------------------------------------------

newForm :: AcidState Blog -> ServerPart Response
newForm acid = createForm acid (BlogPost (PostId 0) "" "") "/create_post"

handleNewForm :: AcidState Blog -> ServerPart Response
handleNewForm acid =
   do post_data <- getDataFn postRq
      case post_data of
        Left e -> badRequest (toResponse (unlines e))
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
        Left e -> badRequest (toResponse (unlines e))
        Right(post_title, post_content, post_id) -> 
                    do post <- update' acid (UpdatePost (BlogPost (PostId post_id) post_title post_content))
                       seeOther ("/posts/" ++ show post_id) (toResponse ())
------------------------------------------ POST UPDATE -------------------------------------------------

-------------------------------------------- TEMPLATE --------------------------------------------------
appTemplate :: String -> [H.Html] -> H.Html -> H.Html
appTemplate title headers body =
    H.html $ do
      H.head $ do
        H.title (H.toHtml title)
        H.meta H.! A.httpEquiv "Content-Type"
               H.! A.content "text/html;charset=utf-8"
        sequence_ headers
      H.body $ do
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
          (do H.h1 (H.toHtml ("Showing " ++ post_title))
              H.p (H.toHtml post_content)
              buildBackLink
              buildDeleteLink key
          )
    ))

buildBackLink :: H.Html
buildBackLink = H.a H.! A.href (stringValue "/allPosts") $ "Back"

buildDeleteLink :: PostId -> H.Html
buildDeleteLink (PostId key) = H.form
                                H.! A.method "POST"
                                H.! A.action (stringValue ("/posts/delete/" ++ show key)) $ do
                                   H.input H.! A.type_ "submit" H.! A.value "Delete"

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
        appTemplate "Programación Funcional"
          []
          (do H.h1 "All posts"
              H.ul $ forM_ posts (H.li . (\(BlogPost key title content) -> H.a H.! (buildLink key) $ H.toHtml title))
              H.a H.! A.href (stringValue "/upload") $ "New Post"
          )
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
              (H.p $ H.toHtml ("A ver... " ++ show (length posts)))
------------------------------------------ HOME --------------------------------------------------------



