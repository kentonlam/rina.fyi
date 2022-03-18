--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Data.Monoid (mappend)
import Hakyll
import qualified Hakyll.Core.Configuration as Config

--------------------------------------------------------------------------------
config :: Config.Configuration
config =
  Config.defaultConfiguration
    { destinationDirectory = "docs",
      providerDirectory = "site"
    }

main :: IO ()
main = hakyllWith config $
  do
    create ["CNAME"] $ do
      route idRoute
      compile $ makeItem @String "rina.fyi"

    match "images/*" $
      do
        route idRoute
        compile copyFileCompiler
    match "css/*" $
      do
        route idRoute
        compile copyFileCompiler
    match (fromList ["about.rst", "contact.markdown"]) $
      do
        route $ setExtension "html"
        compile $
          pandocCompiler
            >>= loadAndApplyTemplate "templates/with-title.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
    match "posts/*" $
      do
        route $ setExtension "html"
        compile $
          pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/with-title.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls
    create ["archive.html"] $
      do
        route idRoute
        compile $
          do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                  listField "posts" postCtx (return posts)
                    `mappend` constField "title" "Archives"
                    `mappend` defaultContext
            makeItem ""
              >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
              >>= loadAndApplyTemplate "templates/with-title.html" archiveCtx
              >>= loadAndApplyTemplate "templates/default.html" archiveCtx
              >>= relativizeUrls
    match "index.md" $
      do
        route $ setExtension "html"
        compile $
          do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                  listField "posts" postCtx (return posts)
                    `mappend` defaultContext
            pandocCompiler
              >>= loadAndApplyTemplate "templates/default.html" indexCtx
              >>= relativizeUrls
    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext
