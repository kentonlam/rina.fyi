{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad

import Hakyll
import qualified Hakyll.Core.Configuration as Config

import Dir
import Lib
import Pandoc


--------------------------------------------------------------------------------
config :: Config.Configuration
config =
  Config.defaultConfiguration
    { destinationDirectory = "docs",
      providerDirectory = "site"
    }

indexCont = doPandoc
  >=> loadAndApplyTemplate "templates/default.html" context
postCont = doPandoc
  >=> loadAndApplyTemplate "templates/post.html" context
  >=> loadAndApplyTemplate "templates/default.html" context

base = "rina.fyi"

main :: IO ()
main = hakyllWith config $
  do
    create [".nojekyll"] makeEmptyRule

    match "css/*" copyRule

    dir "p" context indexCont postCont
    dir "b" context indexCont postCont

    match ("root" /**?/ "*") $
      do
        route $ gsubRoute "root/" (const "")
        compile $ copyFileCompiler

    match "*.md" $
      do
        route $ setExtension "html"
        compile $
          do
            pandoc
              >>= loadAndApplyTemplate "templates/default.html" context

    match "templates/*" $
      do
        compile $
          getResourceBody
            >>= saveSnapshot "raw"
            >>= compileTemplateItem
            >>= makeItem

    create ["CNAME"] $ makeTextRule base
    create ["sitemap.txt"] $
      do
        route idRoute
        compile $
          do
            let ctx = listField "files"
                      (constField "base" base <> context)
                      (loadAll ("*.md" .||. "**/*.md"))
            makeEmpty
              >>= loadAndApplyTemplate "templates/sitemap.txt" ctx
