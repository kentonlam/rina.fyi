module Lib where

import Text.Pandoc.Options

pandocReaderOptions :: ReaderOptions
pandocReaderOptions = def {
    readerExtensions = pandocExtensions,
    readerStandalone = True
}

pandocWriterOptions :: WriterOptions
pandocWriterOptions = def { 

    writerHTMLMathMethod = KaTeX defaultKaTeXURL,
    writerTableOfContents = True,
    writerWrapText = WrapPreserve
    }