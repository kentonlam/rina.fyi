{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad

import Hakyll
import qualified Hakyll.Core.Configuration as Config

config :: Config.Configuration
config =
  Config.defaultConfiguration
    { destinationDirectory = "dir-build",
      providerDirectory = "dir-site"
    }

recGlob :: FilePath -> FilePath -> Pattern
recGlob p f = fromGlob (p <> "/**/" <> f) .||. fromGlob (p <> "/" <> f)

(/**?/) = recGlob

d /./ f = fromGlob (d <> "/" <> f)
d /*/ f = fromGlob (d <> "/*/" <> f)

adjFiles :: Identifier -> Pattern
adjFiles i = (d /./ "*.md" .||. d /*/ "index.md") .&&. complement (fromList [i])
  where d = dirname (toFilePath i)

dir :: String -> Rules ()
dir p = do
  match (p /**?/ "index.md") $ 
    do
      route $ setExtension "html"
      compile $ 
        do
          adj <- adjFiles <$> getUnderlying
          files <- loadAll (adj)
          -- let f' = filter ((/= f) . toFilePath . itemIdentifier) files
          -- unsafeCompiler $ print files
          getResourceBody 
            >>= applyAsTemplate (listCtx files)
            >>= renderPandoc
          -- makeItem $ f
  match (p /**?/ "*.md") $
    do
      route $ setExtension "post.html"
      compile $ getResourceBody

  -- match (p /**?/ "*.md") $
  --   do 
  --     route $ setExtension "html"
  --     compile $ 
  --       do
  --         i <- getUnderlying
  --         item <- load (setVersion (Just "dir") i) :: Compiler (Item String)
  --         makeItem $ itemBody item
  -- create ["index.md"] $
  --   do
  --     route $ setExtension "html"
  --     compile $ makeItem ("make"::String)

main = hakyllWith config (dir "d")

dirname :: FilePath -> FilePath
dirname = reverse . dropWhile (== '/') . dropWhile (/= '/') . reverse

postCtx = defaultContext

listCtx :: [Item String] -> Context String
listCtx items = listField "list" postCtx (pure items) <> postCtx