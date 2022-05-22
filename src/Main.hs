{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where


import Lib
import Hakyll
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
    match ("CNAME" .||. "kumiko.*") $ do
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
    match "p/assets/*" $ do
      route idRoute
      compile copyFileCompiler 
    match "p/*.md" $
      do
        route $ setExtension "html"
        compile $
          pandoc
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            -- >>= loadAndApplyTemplate "templates/with-title.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            -- >>= relativizeUrls
    create ["posts.html"] $
      do
        route idRoute
        compile $
          do
            archiveCtx <- listCtx <$> (recentFirst =<< loadAll "p/*.md")
            -- load "templates/post-list.html"
            -- t <- load "templates/post-list.html"
            -- s <- getMetadataField' "templates/post-list.html" "title"
            loadInitialTemplate "templates/post-list.html" archiveCtx
              -- >>= loadAndApplyTemplate "templates/with-title.html" archiveCtx
              >>= loadAndApplyTemplate "templates/default.html" archiveCtx
              >>= makeItem . itemBody 
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
    match "templates/*" $ 
      do
        compile $ 
          getResourceBody
            >>= saveSnapshot "raw"
            >>= compileTemplateItem 
            >>= makeItem

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = (mapContext trimHTML $ urlField "url") <> dateField "date" "%F" <> defaultContext

listCtx :: [Item String] -> Context String
listCtx items = listField "list" postCtx (pure items) <> postCtx

loadInitialTemplate :: Identifier -> Context String -> Compiler (Item String)
loadInitialTemplate i ctx = loadSnapshot i "raw" >>= applyAsTemplate ctx

-- obtain metadata from the given file into the context. adapted from metadataField
metadataFrom :: Identifier -> Context a
metadataFrom id = Context $ \k _ _ -> do
    let empty' = noResult $ "No '" ++ k ++ "' field in metadata " ++
            "of item " ++ show id
    value <- getMetadataField id k
    maybe empty' (return . StringField) value