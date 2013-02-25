{-# LANGUAGE OverloadedStrings #-}
module HTML (userPage) where

import qualified Data.Text as T
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as HTML
import qualified Text.Blaze.Html5.Attributes as A

import Control.Monad


stylesheet :: T.Text
stylesheet =
  T.concat $ map (\line -> T.concat [line, "\n"])
    ["body {",
     "  margin: 0;",
     "  padding: 0;",
     "  background-color: #c0ffc0",
     "}",
     "",
     "ul#source-list {",
     "  margin: 0;",
     "  padding: 0;",
     "  position: fixed;",
     "  height: 100%;",
     "  width: 20vw;",
     "  top: 0;",
     "  bottom: 0;",
     "  overflow-y: auto;",
     "  overflow-x: hidden;",
     "  background-color: #e0e0ff;",
     "}",
     "",
     "div#content {",
     "  margin: 0;",
     "  padding: 0;",
     "  position: fixed;",
     "  height: 100%;",
     "  width: 80vw;",
     "  right: 0;",
     "  top: 0;",
     "  bottom: 0;",
     "  overflow-x: hidden;",
     "  overflow-y: auto;",
     "}",
     "",
     "div#content > div {",
     "  margin: 1em;",
     "  border-radius: 1em;",
     "  padding: 1em;",
     "  padding-top: 0;",
     "  border: solid black 1.5pt;",
     "  background-image: -webkit-linear-gradient(",
     "      top,",
     "      rgba(209, 241, 209, 0.95) 0%,",
     "      rgba(192, 209, 192, 0.95) 3.0em,",
     "      rgba(192, 209, 192, 0.0) 3.0001em,",
     "      rgba(192, 209, 192, 0.0) 100%",
     "    );",
     "}",
     "",
     "div#content > div > h1,",
     "div#content > div > input[type=submit] {",
     "  font-size: 175%;",
     "}",
     "",
     "div#content > div > h1 {",
     "  margin: 0;",
     "  margin-top: 0.25em;",
     "  margin-bottom: 0.75em;",
     "  padding-left: -1em;",
     "  padding-right: -1em;",
     "}",
     "",
     "div#content > div > h1 > input,",
     "div#content > div > h1 > select,",
     "div#content > div > h1 > select > option {",
     "  font-size: inherit;",
     "}",
     "",
     "div#content > div > h1 > input.url {",
     "  width: auto;",
     "}",
     "",
     "div#content > div > div.buttons > input[type=submit] {",
     "  -webkit-appearance: button;",
     "}",
     "",
     "div#content > div > h1 > select {",
     "  -webkit-appearance: menulist-button;",
     "}"]


userPage :: HTML.Html
userPage = HTML.docTypeHtml $ do
  HTML.head $ do
    HTML.meta ! A.httpEquiv "Content-Type"
              ! A.content "text/html; charset=utf-8"
    HTML.title "Direct Web Bugs"
    HTML.style $ HTML.toMarkup stylesheet
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
            HTML.ul $ do
              HTML.li ! A.class_ "request" $ "GET ireneknapp.com"
              HTML.li ! A.class_ "request" $ "GET ireneknapp.com"
              HTML.li ! A.class_ "request" $ "GET ireneknapp.com"
              HTML.li ! A.class_ "request" $ "GET ireneknapp.com"
    HTML.div ! A.id "content" $ do
      HTML.div ! A.class_ "request" $ do
        HTML.h1 $ do
          HTML.select ! A.class_ "method" $ do
            HTML.option $ "DELETE"
            HTML.option ! A.selected "" $ "GET"
            HTML.option $ "POST"
            HTML.option $ "PUT"
          " "
          HTML.input ! A.class_ "url"
                     ! A.required ""
                     ! A.size "40"
                     ! A.placeholder "ireneknapp.com/foo/bar/baz.html"
        HTML.p $ do
          "Lorem ipsum dolor sit amet, consectetur adipiscing elit. "
          "Sed id turpis id nisl aliquet dignissim in ut lectus. "
          "Nullam tortor erat, convallis sit amet hendrerit a, "
          "tincidunt sed erat. Maecenas iaculis cursus diam. Quisque "
          "ac nunc lorem. Maecenas dictum volutpat odio nec facilisis. "
          "Integer sagittis tincidunt leo ac dictum. Vestibulum pulvinar "
          "fringilla tellus, non pretium odio auctor sit amet."
        HTML.div ! A.class_ "buttons" $ do
          HTML.input ! A.class_ "do-it"
                     ! A.type_ "submit"
                     ! A.value "Do it!"
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
