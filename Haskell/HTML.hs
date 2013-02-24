{-# LANGUAGE OverloadedStrings #-}
module HTML (userPage) where

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as HTML
import qualified Text.Blaze.Html5.Attributes as A

import Control.Monad


userPage :: HTML.Html
userPage = HTML.docTypeHtml $ do
  HTML.head $ do
    HTML.meta ! A.httpEquiv "Content-Type"
              ! A.content "text/html; charset=utf-8"
    HTML.title "Direct Web Bugs"
  HTML.body $ do
    HTML.ul ! A.id "source-list" $ do
      HTML.li ! A.class_ "folder recent-requests expanded" $ do
        "Recent requests"
        HTML.ul $ do
          HTML.li ! A.class_ "request" $ "GET ireneknapp.com"
          HTML.li ! A.class_ "request" $ "GET ireneknapp.com"
          HTML.li ! A.class_ "request" $ "GET ireneknapp.com"
          HTML.li ! A.class_ "request" $ "GET ireneknapp.com"
      HTML.li ! A.class_ "folder expanded" $ do
        "Example folder"
        HTML.ul $ do
          HTML.li ! A.class_ "request" $ "GET ireneknapp.com"
          HTML.li ! A.class_ "request" $ "GET ireneknapp.com"
          HTML.li ! A.class_ "folder collapsed" $ do
            "Example subfolder" 
            HTML.li ! A.class_ "request" $ "GET ireneknapp.com"
            HTML.li ! A.class_ "request" $ "GET ireneknapp.com"
            HTML.li ! A.class_ "request" $ "GET ireneknapp.com"
            HTML.li ! A.class_ "request" $ "GET ireneknapp.com"
    HTML.div ! A.id "content" $ do
      HTML.div ! A.class_ "request" $ do
        HTML.h1 $ do
          "Request "
          HTML.input
        HTML.p $ do
          "Lorem ipsum dolor sit amet, consectetur adipiscing elit. "
          "Sed id turpis id nisl aliquet dignissim in ut lectus. "
          "Nullam tortor erat, convallis sit amet hendrerit a, "
          "tincidunt sed erat. Maecenas iaculis cursus diam. Quisque "
          "ac nunc lorem. Maecenas dictum volutpat odio nec facilisis. "
          "Integer sagittis tincidunt leo ac dictum. Vestibulum pulvinar "
          "fringilla tellus, non pretium odio auctor sit amet."
      HTML.div ! A.class_ "response" $ do
        HTML.h1 $ do
          "Response from ireneknapp.com"
        HTML.p $ do
          "Lorem ipsum dolor sit amet, consectetur adipiscing elit. "
          "Sed id turpis id nisl aliquet dignissim in ut lectus. "
          "Nullam tortor erat, convallis sit amet hendrerit a, "
          "tincidunt sed erat. Maecenas iaculis cursus diam. Quisque "
          "ac nunc lorem. Maecenas dictum volutpat odio nec facilisis. "
          "Integer sagittis tincidunt leo ac dictum. Vestibulum pulvinar "
          "fringilla tellus, non pretium odio auctor sit amet."
      HTML.div ! A.class_ "response" $ do
        HTML.h1 $ do
          "Response from ireneknapp.com"
        HTML.p $ do
          "Lorem ipsum dolor sit amet, consectetur adipiscing elit. "
          "Sed id turpis id nisl aliquet dignissim in ut lectus. "
          "Nullam tortor erat, convallis sit amet hendrerit a, "
          "tincidunt sed erat. Maecenas iaculis cursus diam. Quisque "
          "ac nunc lorem. Maecenas dictum volutpat odio nec facilisis. "
          "Integer sagittis tincidunt leo ac dictum. Vestibulum pulvinar "
          "fringilla tellus, non pretium odio auctor sit amet."
