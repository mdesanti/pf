{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative  ((<$>), (<*>))
import Control.Monad (msum)
import Happstack.Server(Method(GET, HEAD), dir, methodM, ServerPart, Response,
                        toResponse, simpleHTTP, nullConf, ok, toMessage, look,
                        defaultBodyPolicy, BodyPolicy, decodeBody, RqData,
                        getDataFn, badRequest)
import           Text.Blaze ((!))
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A


myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)

helloRq :: RqData (String, String)
helloRq =
    (,) <$> look "start" <*> look "end"

main :: IO ()
main = simpleHTTP nullConf $
        do decodeBody myPolicy
           msum [ do dir "blaze" $ do methodM [GET, HEAD] 
                     helloBlaze,
                  do dir "home" $ do methodM [GET] 
                     handleHome
                ]

handleHome :: ServerPart Response
handleHome = 
    do r <- getDataFn helloRq
       case r of
          Left e ->
            badRequest $ toResponse $ unlines e
          Right(start, end) -> home start end

home :: String -> String -> ServerPart Response
home start end = ok $ toResponse $
                      appTemplate "Programación Funcional"
                        [H.meta ! A.name "keywords"
                                ! A.content "happstack, blaze, html"
                        ]
                        (H.p $ H.toHtml ("You have requested a search from " ++ start ++ " to " ++ end))




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

helloBlaze :: ServerPart Response
helloBlaze =
   ok $ toResponse $
    appTemplate "Hello, Blaze!"
                [H.meta ! A.name "keywords"
                        ! A.content "happstack, blaze, html"
                ]
                (H.p $ do "Hello, "
                          H.b "blaze-html!")










