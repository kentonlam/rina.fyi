{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Foldable

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

loadAdj :: Identifier -> Compiler ([Item String], [Item String], [Item String])
loadAdj i = (,,) <$> l (adjFiles i) <*> l (adjDirs i) <*> l (adjFiles i .||. adjDirs i)
  where l = loadAll

sortOptions :: String -> [Item String] -> Compiler (Context String)
sortOptions k items = do
  recent <- recentFirst items
  chrono <- chronological items
  pure $ lf k items <> lf (k <> "-recent") recent <> lf (k <> "-chrono") chrono
  where lf k f = listField k postCtx (pure f)

-- listFunctionField :: String                                  -- ^ Key
--                   -> ([String] -> [Item a] -> Compiler String) -- ^ Function
--                   -> Context a

-- fieldWithArgs :: String -> ([String] -> Item a -> Compiler ContextField) -> Context a
-- fieldWithArgs key value = Context $ \k a i ->
--     if k == key
--         then value a i
--         else noResult $ "Tried field " ++ key


-- listFunctionField :: String -> ([Item a] -> Compiler [Item a]) -> Context a
-- listFunctionField k f = fieldWithArgs k (\a _ -> do
--     )

dir :: String -> Rules ()
dir p = do
  match (p /**?/ "index.md") $ 
    do
      route $ setExtension "html"
      compile $ 
        do
          i <- getUnderlying

          (files, dirs, both) <- loadAdj i
          -- let f' = filter ((/= f) . toFilePath . itemIdentifier) files
          -- unsafeCompiler $ print files
          -- [Compiler (Context a)]
          lists <- fold <$> sequence [sortOptions "files" files, sortOptions "dirs" dirs, sortOptions "both" both]
          -- let lf k f = listField k postCtx (pure f)
          let ctx = lists <> postCtx
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

postCtx :: Context String
postCtx = dateField "date" "%F" <> defaultContext

listCtx :: [Item String] -> Context String
listCtx items = listField "list" postCtx (pure items) <> postCtx