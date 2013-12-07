{-# LANGUAGE OverloadedStrings, CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}

module Main where

import Control.Applicative  ((<$>), (<*>))
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader ( ask )
import Control.Exception    ( bracket )
import Happstack.Server(Method(GET, HEAD, POST), dir, methodM, ServerPart, Response,
                        toResponse, simpleHTTP, nullConf, ok, toMessage, look, lookRead,
                        defaultBodyPolicy, BodyPolicy, decodeBody, RqData,
                        getDataFn, badRequest, lookFile, path, resp)
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
         , posts      = empty
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


allPosts :: Query Blog  [BlogPost]
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

$(makeAcidic ''Blog ['addPost, 'allPosts, 'getPost, 'updatePost])

------------------------------------------ ACID CONFIGURATION ------------------------------------------
------------------------------------------ MAIN --------------------------------------------------------

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" (10*10^6) 1000 1000)

main :: IO ()
main = 
  do updateGlobalLogger rootLoggerName (setLevel INFO)
     bracket (openLocalState initialBlogState)
             (createCheckpointAndClose)
              (\acid -> simpleHTTP nullConf $
                          do decodeBody myPolicy
                             msum [ do dir "upload" $ do methodM [GET, HEAD] 
                                       newForm acid,
                                    do dir "create_post" $ do methodM [POST]
                                       handleNewForm acid,
                                    do dir "update_post" $ do methodM [POST]
                                       handleEditForm acid,
                                    do dir "allPosts" $ do methodM [GET]
                                       handleAllPosts acid,
                                    do dir "posts" $ do path $ (\s -> showPost acid s),
                                    do dir "update_post" $ do path $ (\s -> editForm acid s),
                                    home acid
                                  ])
------------------------------------------ MAIN --------------------------------------------------------

------------------------------------------ POST UPLOAD -------------------------------------------------

postRq :: RqData (String, String, Integer)
postRq =
    (,,) <$> look "post_title" <*> look "post_content" <*> lookRead "post_id"

newForm :: AcidState Blog -> ServerPart Response
newForm acid = createForm acid (BlogPost (PostId 0) "" "") "/create_post"


createForm :: AcidState Blog -> BlogPost -> String -> ServerPart Response
createForm acid (BlogPost (PostId key) title content) post_url = ok $ toResponse $
    appTemplate "Programación Funcional" []
      (H.form ! A.enctype "multipart/form-data"
            ! A.method "POST"
            ! A.action (stringValue post_url) $ do
               H.label "Post Title"
               H.input ! A.type_ "text" ! A.name "post_title" ! A.value (stringValue title)
               H.label "Post Content"
               H.textarea ! A.type_ "text" ! A.name "post_content" ! A.cols (H.toValue (60 ::Integer)) ! A.rows (H.toValue (10 ::Integer)) $ (H.toHtml content)
               H.input ! A.type_ "hidden" ! A.name "post_id" ! A.value (stringValue (show key))
               H.input ! A.type_ "submit" ! A.value "upload"
      )


handleNewForm :: AcidState Blog -> ServerPart Response
handleNewForm acid =
   do post_data <- getDataFn postRq
      case post_data of
        Left e -> badRequest (toResponse (unlines e))
        Right(post_title, post_content, post_id) -> 
                    do c <- update' acid (AddPost post_title post_content)
                       ok $ toResponse $ 
                         appTemplate "Programación Funcional" [] (mkBody post_title post_content)      
                           where
                             mkBody post_title post_content = do
                               H.p (H.toHtml $ "Post Title: " ++ post_title)
                               H.p (H.toHtml $ "Post Content:  " ++ post_content)
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
                    do c <- update' acid (UpdatePost (BlogPost (PostId post_id) post_title post_content))
                       ok $ toResponse $ 
                         appTemplate "Programación Funcional" [] (mkBody post_title post_content)      
                           where
                             mkBody post_title post_content = do
                               H.p (H.toHtml $ "Post Title: " ++ post_title)
                               H.p (H.toHtml $ "Post Content:  " ++ post_content)
------------------------------------------ POST UPDATE -------------------------------------------------

-------------------------------------------- TEMPLATE --------------------------------------------------
appTemplate :: String -> [H.Html] -> H.Html -> H.Html
appTemplate title headers body =
    H.html $ do
      H.head $ do
        H.title (H.toHtml title)
        H.meta ! A.httpEquiv "Content-Type"
               ! A.content "text/html;charset=utf-8"
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
buildShowResponse (BlogPost _ post_title post_content) = 
  ok (toResponse (
        appTemplate "Programación Funcional"
          []
          (do H.h1 (H.toHtml ("Showing " ++ post_title))
              H.p (H.toHtml post_content)
          )
    ))

------------------------------------------ SHOW ONE POST ----------------------------------------------

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
              H.ul $ forM_ posts (H.li . (\(BlogPost key title content) -> H.a ! (buildLink key) $ H.toHtml title)))
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
              [H.meta ! A.name "keywords"
                      ! A.content "happstack, blaze, html"
              ]
              (H.p $ H.toHtml ("A ver... " ++ show (length posts)))
------------------------------------------ HOME --------------------------------------------------------



