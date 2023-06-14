module Lib where

import Control.Monad
import Data.List

import Hakyll
import Hakyll.Core.Routes
import Data.Char (toLower)

makeEmpty = makeItem (""::String)

copyRule = do
  route idRoute
  compile copyFileCompiler

makeTextRule :: String -> Rules ()
makeTextRule s = do
  route idRoute
  compile (makeItem s)

makeEmptyRule = makeTextRule ""


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

getStringField :: String -> Context a -> Item a -> Compiler String
getStringField k c = unContext c k [] >=> matchString

mapField :: String -> (String -> String) -> Context a -> Context a
mapField k f c = mapField2 k (const f) c

mapField2 :: String -> (Item a -> String -> String) -> Context a -> Context a
mapField2 k f c = field k (\i -> f i <$> getStringField k c i)


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
trimSuffix suf = replaceSuffix suf ""

replaceSuffix :: String -> String -> String -> String
replaceSuffix suf rep s = maybe s (++ rep) $ r <$> stripPrefix (r suf) (r s)
  where r = reverse

replace :: Eq a => a -> a -> a -> a
replace old new s
  | s == old = new
  | otherwise = s

cleanURL :: String -> String
cleanURL = replace "/" "" . trimSuffix ".html" . replaceSuffix "/index.html" "/"

lookupBool :: String -> Metadata -> Maybe Bool
lookupBool key meta = (== "true" ) . map toLower <$> lookupString key meta
