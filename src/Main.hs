{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad

import Hakyll
import qualified Hakyll.Core.Configuration as Config

import Dir
import Lib


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


main :: IO ()
main = hakyllWith config $
  do
    create [".nojekyll"] makeEmptyRule

    match (fromList ["CNAME", "kumiko.png"]) copyRule
    match "css/*" copyRule

    dir "p" context indexCont postCont
    dir "b" context indexCont postCont
    
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

