{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}

module Javascript where

import           Data.Text (Text)
import           GHC.Exts
import           GI.Gio.Objects.Cancellable (noCancellable)
import qualified GI.WebKit2 as WK2
import           Text.InterpolatedString.QQ2

newtype Javascript = Javascript Text
  deriving (IsString, Semigroup, Monoid)


jsScrollTop :: Javascript
jsScrollTop = "window.scrollTo(0, 0);"

jsScrollBottom :: Javascript
jsScrollBottom = "window.scrollTo(0, 9999999);"

jsScroll :: Int -> Javascript
jsScroll lines = [qc|"window.scrollTo(0, 14 * lines);"|]

runJS :: WK2.WebView -> Javascript -> IO ()
runJS wv (Javascript js) = WK2.webViewRunJavascript wv js noCancellable Nothing
