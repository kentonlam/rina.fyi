{-# LANGUAGE OverloadedStrings #-}

module Dir where

import Control.Monad
import Data.Foldable

import Hakyll

import Lib

(/**?/) :: FilePath -> FilePath -> Pattern
d /**?/ f = fromGlob (d <> "/**/" <> f) .||. fromGlob (d <> "/" <> f)

d /./ f = fromGlob (d <> "/" <> f)
d /*/ f = fromGlob (d <> "/*/" <> f)

dirname :: FilePath -> FilePath
dirname = reverse . dropWhile (== '/') . dropWhile (/= '/') . reverse


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

sortOptions :: Context String -> String -> [Item String] -> Compiler (Context String)
sortOptions ctx k items = do
  recent <- recentFirst items
  chrono <- chronological items
  pure $ lf k items <> lf (k <> "-recent") recent <> lf (k <> "-chrono") chrono
  where lf k f = listField k ctx (pure f)


dir :: String -> Context String -> (Item String -> Compiler (Item String)) -> (Item String -> Compiler (Item String)) -> Rules ()
dir p c indexCont postCont = do
  match (p /**?/ "index.md") $ 
    do
      route $ setExtension "html"
      compile $ 
        do
          (files, dirs, both) <- loadAdj =<< getUnderlying
          -- let f' = filter ((/= f) . toFilePath . itemIdentifier) files
          -- unsafeCompiler $ print files
          -- [Compiler (Context a)]
          lists <- fold <$> sequence [sortOptions c "files" files, sortOptions c "dirs" dirs, sortOptions c "both" both]
          -- let lf k f = listField k postCtx (pure f)
          let ctx = lists <> c
          getResourceBody 
            >>= applyAsTemplate ctx
            >>= indexCont
          -- makeItem $ f
  match (p /**?/ "*.md") $
    do
      route $ setExtension "html"
      compile $ getResourceBody 
        >>= postCont

  match (p /**?/ "*" .&&. complement (p /**?/ "*.md")) $
    do
      route idRoute
      compile copyFileCompiler

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

-- main = hakyllWith config (dir "d")
