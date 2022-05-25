{-# LANGUAGE TypeApplications #-}

module Lib where

import Data.List

import Hakyll
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

doPandoc = renderPandocWith pandocReaderOptions pandocWriterOptions 
pandoc = pandocCompilerWith pandocReaderOptions pandocWriterOptions 

makeEmpty = makeItem @String ""


copyRule = do
  route idRoute
  compile copyFileCompiler

makeEmptyRule = do
  route idRoute
  compile makeEmpty
  

(.<>) :: (Context a -> Context a) -> Context a -> Context a
f .<> c = f c <> c

infixr 5 .<>

context :: Context String
context = mapField2 "title" cleanTitle 
  .<> mapField "url" cleanURL 
  .<> dateField "date" "%F" 
  <> defaultContext

matchString :: ContextField -> Compiler String
matchString f = case f of
    EmptyField      -> wrongType "boolField"
    StringField str -> pure str
    _               -> wrongType "ListField"
  where
    wrongType typ = fail $ "matchString: " ++
        "expected StringField but got " ++ typ ++ "!"

mapField :: String -> (String -> String) -> Context a -> Context a
mapField k f c = field k (\i -> unContext c k [] i >>= matchString >>= pure . f)

mapField2 :: String -> (Item a -> String -> String) -> Context a -> Context a
mapField2 k f c = field k (\i -> unContext c k [] i >>= matchString >>= pure . f i)


itemPath :: Item a -> FilePath
itemPath = toFilePath . itemIdentifier

cleanTitle :: Item a -> String -> String
cleanTitle i "index" | basename p == "index.md" = basename (dirname p) <> "/"
  where p = itemPath i
cleanTitle _ t = t

{-
:def go (const $ return ":r\n:main rebuild")
-}

dirname :: FilePath -> FilePath
dirname = reverse . dropWhile (== '/') . dropWhile (/= '/') . reverse

basename :: FilePath -> FilePath
basename = reverse . takeWhile (/= '/') . reverse

trimSuffix :: String -> String -> String
trimSuffix suf s = maybe s r $ stripPrefix (r suf) (r s)
    where r = reverse

cleanURL :: String -> String
cleanURL = trimSuffix ".html" . trimSuffix "/index.html"
