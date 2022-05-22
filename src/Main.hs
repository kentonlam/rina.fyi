{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where


import Lib
import Hakyll
import Data.Monoid (mappend)
import qualified Hakyll.Core.Configuration as Config

pandoc = pandocCompilerWith pandocReaderOptions pandocWriterOptions

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
      compile copyFileCompiler

    -- match "images/*" $
    --   do
    --     route idRoute
    --     compile copyFileCompiler
    match "css/*" $
      do
        route idRoute
        compile copyFileCompiler

    -- match (fromList ["about.rst", "contact.markdown"]) 
    --   do
    --     route $ setExtension "html"
    --     compile $
    --       pandocCompiler
    --         >>= loadAndApplyTemplate "templates/with-title.html" defaultContext
    --         >>= loadAndApplyTemplate "templates/default.html" defaultContext
    --         >>= relativizeUrls
    match "p/assets/*" $ route idRoute >> compile copyFileCompiler 
    match "p/*.md" $
      do
        route $ setExtension "html"
        compile $
          pandoc
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            -- >>= loadAndApplyTemplate "templates/with-title.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            -- >>= relativizeUrls
    -- create ["archive.html"] $
    --   do
    --     route idRoute
    --     compile $
    --       do
    --         posts <- recentFirst =<< loadAll "posts/*"
    --         let archiveCtx =
    --               listField "posts" postCtx (return posts)
    --                 `mappend` constField "title" "Archives"
    --                 `mappend` defaultContext
    --         makeItem ""
    --           >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
    --           >>= loadAndApplyTemplate "templates/with-title.html" archiveCtx
    --           >>= loadAndApplyTemplate "templates/default.html" archiveCtx
    --           >>= relativizeUrls
    match "index.md" $
      do
        route $ setExtension "html"
        compile $
          do
            -- posts <- recentFirst =<< loadAll "posts/*"
            -- let indexCtx =
            --       listField "posts" postCtx (return posts)
            --         `mappend` defaultContext
            pandoc
              >>= loadAndApplyTemplate "templates/default.html" postCtx
    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext