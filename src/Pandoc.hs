{-# LANGUAGE OverloadedStrings #-}

module Pandoc where



import Text.Pandoc.Options
import Text.Pandoc.Filter
import Text.Pandoc.Definition
import Text.Pandoc.Walk ( walk )

import qualified Data.Text as T

import Hakyll

extensions = Ext_autolink_bare_uris .+ Ext_smart .+ pandocExtensions
  where
    (.+) = enableExtension
    infixr 2 .+

pandocReaderOptions :: ReaderOptions
pandocReaderOptions = def {
    readerExtensions = extensions,
    readerStandalone = True
}

pandocWriterOptions :: WriterOptions
pandocWriterOptions = def {
    writerExtensions = extensions,
    writerHTMLMathMethod = KaTeX defaultKaTeXURL,
    writerTableOfContents = True,
    writerWrapText = WrapPreserve
}

anchorLinks :: Block -> Block
anchorLinks a@(Header l attrs@(i,_,_) text) | not (T.null i) =
  Header l attrs (text ++ [Space, link])
  where
    link = Link
      ("", ["anchor"], [("aria-hidden", "true")])
      [Str "#"]
      ("#" <> i, "")
anchorLinks x = x

pandocTransform = walk anchorLinks

doPandoc = renderPandocWithTransform pandocReaderOptions pandocWriterOptions pandocTransform
pandoc = pandocCompilerWithTransform pandocReaderOptions pandocWriterOptions pandocTransform
