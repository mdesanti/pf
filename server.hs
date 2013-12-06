{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (msum)
import Happstack.Server(Method(GET, HEAD), dir, methodM, ServerPart, Response,
                        toResponse, simpleHTTP, nullConf, ok, toMessage)
import           Text.Blaze ((!))
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

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

home :: ServerPart Response
home =
   ok $ toResponse $
    appTemplate "Programación Funcional"
                [H.meta ! A.name "keywords"
                        ! A.content "happstack, blaze, html"
                ]
                (H.p $ do "Trabajo práctico especial. 2013 - Segundo Cuatrimestre "
                          H.b "Matías De Santi, Esteban Pintos")

main :: IO ()
main = simpleHTTP nullConf $ msum
       [ do dir "home" $ do methodM [GET, HEAD] 
            helloBlaze,
         do methodM [GET]
            home
       ]