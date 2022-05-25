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

notIden :: Identifier -> Pattern
notIden = complement . fromList . pure

adjFiles :: Identifier -> Pattern
adjFiles i = d /./ "*.md" .&&. notIden i
  where d = dirname (toFilePath i)

adjDirs :: Identifier -> Pattern
adjDirs i = d /*/ "index.md" .&&. notIden i 
  where d = dirname (toFilePath i)

dir :: String -> Rules ()
dir p = do
  match (p /**?/ "index.md") $ 
    do
      route $ setExtension "html"
      compile $ 
        do
          i <- getUnderlying

          files <- loadAll (adjFiles i)
          dirs <- loadAll (adjDirs i)
          both <- loadAll (adjFiles i .||. adjDirs i)
          -- let f' = filter ((/= f) . toFilePath . itemIdentifier) files
          -- unsafeCompiler $ print files

          let lf k f = listField k postCtx (pure f)
          let ctx = lf "both" both <> lf "dirs" dirs <> lf "files" files <> postCtx
          getResourceBody 
            >>= applyAsTemplate ctx
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

postCtx = dateField "date" "%F" <> defaultContext

listCtx :: [Item String] -> Context String
listCtx items = listField "list" postCtx (pure items) <> postCtx