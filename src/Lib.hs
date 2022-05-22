module Lib where

import Data.List
import Text.Pandoc.Options


pandocReaderOptions :: ReaderOptions
pandocReaderOptions = def {
    readerExtensions = Ext_smart `enableExtension` pandocExtensions,
    readerStandalone = True
}

pandocWriterOptions :: WriterOptions
pandocWriterOptions = def { 
    writerExtensions = Ext_smart `enableExtension` pandocExtensions,
    writerHTMLMathMethod = KaTeX defaultKaTeXURL,
    writerTableOfContents = True,
    writerWrapText = WrapPreserve
}

{-
:def go (const $ return ":r\n:main rebuild")
-}

trimHTML :: String -> String
trimHTML s = maybe "" r $ stripPrefix (r ".html") (r s)
    where r = reverse