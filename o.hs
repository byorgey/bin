
import           Data.Char           (toLower)
import           Data.List

import           Control.Applicative

import           System.Environment
import           System.FilePath
import           System.Process

lower = map toLower

x ==> ys = (x, ys)

exts =
  [ "em" ==>
    [ "hs"
    , "lhs"
    , "txt"
    , "tex"
    , "org"
    , "v"
    ]

  , "display" ==>
    [ "png"
    , "jpg"
    , "jpeg"
    , "bmp"
    , "tiff"
    , "gif"
    ]

  , "evince" ==>
    [ "pdf"
    , "djvu"
    , "dvi"
    ]

  , "gs" ==>
    [ "ps"
    ]

  , "tar -xzf" ==>
    [ "tgz"
    , "tar.gz"
    , "tar.xz"
    ]

  , "gunzip" ==>
    [ "gz"
    ]

  , "tar -xvjf" ==>
    [ "tar.bz2"
    ]

  , "tar -xvf" ==>
    [ "tar"
    ]

  , "unzip" ==>
    [ "zip"
    ]

  , "p7zip -d" ==>
    [ "7z"
    ]

  , "soffice" ==>
    [ "doc"
    , "docx"
    , "xls"
    , "xlsx"
    , "ods"
    , "odt"
    , "ppt"
    , "pptx"
    , "odp"
    , "rtf"
    ]

  , "firefox" ==>
    [ "html"
    , "htm"
    , "svg"
    ]
  ]

main = do
  getArgs >>= mapM_ open

open f =
  case findProg f of
    Nothing -> putStrLn $ "Don't know what to do with " ++ f
    Just p  -> const () <$> system (p ++ " '" ++ f ++ "' &")

findProg :: String -> Maybe String
findProg s = fst <$> find (any (`isSuffixOf` lower s) . snd) exts
