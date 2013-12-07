{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative  ((<$>), (<*>))
import Control.Monad (msum)
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


myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" (10*10^6) 1000 1000)

helloRq :: RqData (String, String)
helloRq =
    (,) <$> look "start" <*> look "end"

main :: IO ()
main = simpleHTTP nullConf $
        do decodeBody myPolicy
           msum [ do dir "upload" $ do methodM [GET, HEAD] 
                     uploadForm,
                  do dir "create_post" $ do methodM [POST]
                     handleForm
                ]


------------------------------------------ POST UPLOAD -------------------------------------------------
uploadForm :: ServerPart Response
uploadForm = ok $ toResponse $
    appTemplate "Programación Funcional" []
      (H.form ! A.enctype "multipart/form-data"
            ! A.method "POST"
            ! A.action "/create_post" $ do
               H.label "Post Title"
               H.input ! A.type_ "text" ! A.name "post_title"
               H.label "Post Content"
               H.textarea ! A.type_ "text" ! A.name "name" ! A.id "name" $ ""
               H.input ! A.type_ "submit" ! A.value "upload"
      )


handleForm :: ServerPart Response
handleForm =
   do title <- look "post_title"
      post_content <- look "post_content"
      ok $ toResponse $
         appTemplate "Programación Funcional" [] (mkBody title post_content)
    where
      mkBody title post_content = do
        H.p (H.toHtml $ "Post Title: " ++ title)
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
--handleHome :: ServerPart Response
--handleHome = 
--    do r <- getDataFn helloRq
--       case r of
--          Left e -> badRequest $ toResponse $ unlines e
--          Right(start, end) -> home start end

--home :: String -> String -> ServerPart Response
--home start end = ok $ toResponse $
--                      appTemplate "Programación Funcional"
--                        [H.meta ! A.name "keywords"
--                                ! A.content "happstack, blaze, html"
--                        ]
--                        (H.p $ H.toHtml ("You have requested a search from " ++ start ++ " to " ++ end))
------------------------------------------ HOME --------------------------------------------------------



