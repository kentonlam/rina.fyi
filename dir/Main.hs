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

adjFiles :: Identifier -> Pattern
adjFiles i = (fromGlob (d <> "/*.md") .||. fromGlob (d <> "/*/index.md")) .&&. complement (fromList [i])
  where d = dirname (toFilePath i)

dir :: String -> Rules ()
dir p = do
  match (p /**?/ "index.md") $ 
    do
      route $ setExtension "html"
      compile $ 
        do
          files <- loadAll <$> adjFiles =<< getUnderlying
          -- let f' = filter ((/= f) . toFilePath . itemIdentifier) files
          -- unsafeCompiler $ print files
          getResourceBody 
            >>= applyAsTemplate (listCtx files)
            >>= renderPandoc
          -- makeItem $ f
  match (p /**?/ "*.md") $ 
    do
      route $ setExtension "post.html"
      compile $
        getResourceBody

main = hakyllWith config (dir "d")

dirname :: FilePath -> FilePath
dirname = reverse . dropWhile (== '/') . dropWhile (/= '/') . reverse

postCtx = defaultContext

listCtx :: [Item String] -> Context String
listCtx items = listField "list" postCtx (pure items) <> postCtx