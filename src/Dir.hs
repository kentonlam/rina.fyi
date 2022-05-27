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
  let
    recent = recentFirst items
    chrono = chronological items
  pure $ lf k (pure items) <> lf (k <> "-recent") recent <> lf (k <> "-chrono") chrono
  where lf k f = listField k ctx f


dir ::
  FilePath
  -> Context String
  -> (Item String -> Compiler (Item String))
  -> (Item String -> Compiler (Item String))
  -> Rules ()
dir p c indexCont postCont = do
  match (p /**?/ "index.md") $
    do
      route $ setExtension "html"
      compile $
        do
          (files, dirs, both) <- loadAdj =<< getUnderlying
          lists <- fold <$> traverse (uncurry $ sortOptions c)
            [("files", files), ("dirs", dirs), ("both", both)]
          getResourceBody
            >>= applyAsTemplate (lists <> c)
            >>= indexCont
  match (p /**?/ "*.md") $
    do
      route $ setExtension "html"
      compile $ getResourceBody
        >>= postCont

  match (p /**?/ "*" .&&. complement (p /**?/ "*.md")) $
    do
      route idRoute
      compile copyFileCompiler

-- main = hakyllWith config (dir "d")
