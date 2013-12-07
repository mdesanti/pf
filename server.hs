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
                        toResponse, simpleHTTP, nullConf, ok, toMessage, look,
                        defaultBodyPolicy, BodyPolicy, decodeBody, RqData,
                        getDataFn, badRequest, lookFile)
import           Text.Blaze ((!))
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
import Data.SafeCopy        ( base, deriveSafeCopy )

------------------------------------------ POST DEFINITION ---------------------------------------------

data BlogPost = BlogPost { title :: String, content :: String } deriving (Eq, Ord, Read, Show, Data, Typeable)

data Posts = Posts { all_posts :: [BlogPost]} deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''BlogPost)
$(deriveSafeCopy 0 'base ''Posts)

initialPostsState :: Posts
initialPostsState = Posts []

addPost :: BlogPost -> Update Posts [BlogPost]
addPost post =
    do c@Posts{..} <- get
       put $ c { all_posts = post:all_posts }
       return all_posts

allPosts :: Query Posts [BlogPost]
allPosts = all_posts <$> ask

$(makeAcidic ''Posts ['addPost, 'allPosts])

------------------------------------------ POST DEFINITION ---------------------------------------------

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" (10*10^6) 1000 1000)

main :: IO ()
main = 
  do updateGlobalLogger rootLoggerName (setLevel INFO)
     bracket (openLocalState initialPostsState)
             (createCheckpointAndClose)
              (\acid -> simpleHTTP nullConf $
                          do decodeBody myPolicy
                             msum [ do dir "upload" $ do methodM [GET, HEAD] 
                                       uploadForm acid,
                                    do dir "create_post" $ do methodM [POST]
                                       handleForm acid,
                                    home acid
                                  ])


------------------------------------------ POST UPLOAD -------------------------------------------------

postRq :: RqData (String, String)
postRq =
    (,) <$> look "post_title" <*> look "post_content"

uploadForm :: AcidState Posts -> ServerPart Response
uploadForm acid = ok $ toResponse $
    appTemplate "Programación Funcional" []
      (H.form ! A.enctype "multipart/form-data"
            ! A.method "POST"
            ! A.action "/create_post" $ do
               H.label "Post Title"
               H.input ! A.type_ "text" ! A.name "post_title"
               H.label "Post Content"
               H.textarea ! A.type_ "text" ! A.name "post_content" ! A.id "name" $ ""
               H.input ! A.type_ "submit" ! A.value "upload"
      )


handleForm :: AcidState Posts -> ServerPart Response
handleForm acid =
   do post_data <- getDataFn postRq
      case post_data of
        Left e -> badRequest (toResponse (unlines e))
        Right(post_title, post_content) -> 
                                          do c <- update' acid (AddPost (BlogPost post_title post_content))
                                             ok $ toResponse $ 
                                               appTemplate "Programación Funcional" [] (mkBody post_title post_content)      
                                                 where
                                                   mkBody post_title post_content = do
                                                     H.p (H.toHtml $ "Post Title: " ++ post_title)
                                                     H.p (H.toHtml $ "Post Content:  " ++ post_content)
------------------------------------------ POST UPLOAD -------------------------------------------------

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


------------------------------------------ HOME --------------------------------------------------------
home :: AcidState Posts -> ServerPart Response
home acid = 
      do posts <- query' acid AllPosts
         ok $ toResponse $
            appTemplate "Programación Funcional"
              [H.meta ! A.name "keywords"
                      ! A.content "happstack, blaze, html"
              ]
              (H.p $ H.toHtml ("A ver... " ++ show (length posts)))
------------------------------------------ HOME --------------------------------------------------------



