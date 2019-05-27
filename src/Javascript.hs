{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}

module Javascript where

import           Data.Text (Text)
import           GHC.Exts
import           GI.Gio.Objects.Cancellable (noCancellable)
import qualified GI.WebKit2 as WK2
import           Text.InterpolatedString.QQ2

newtype Javascript a = Javascript Text
  deriving (IsString, Semigroup, Monoid)
  deriving Show via Text


jsScrollTop :: Javascript ()
jsScrollTop = "window.scrollTo(0, 0);"

jsScrollBottom :: Javascript ()
jsScrollBottom = "window.scrollTo(0, 9999999);"

jsScroll :: Int -> Javascript ()
jsScroll numlines = [qc|window.scrollBy(0, 14 * #{numlines});|]

jsHinting :: Javascript ()
jsHinting = [qc|
function addClientRectsOverlay(elt) {
  /* Absolutely position a div over each client rect so that its border width
     is the same as the rectangle's width.
     Note: the overlays will be out of place if the user resizes or zooms. */
  var rects = elt.getClientRects();
  for (var i = 0; i != rects.length; i++) {
    var rect = rects[i];
    var tableRectDiv = document.createElement('div');
    tableRectDiv.style.position = 'absolute';
    tableRectDiv.style.border = '1px solid red';
    var scrollTop = document.documentElement.scrollTop || document.body.scrollTop;
    var scrollLeft = document.documentElement.scrollLeft || document.body.scrollLeft;
    tableRectDiv.style.margin = tableRectDiv.style.padding = '0';
    tableRectDiv.style.top = (rect.top + scrollTop) + 'px';
    tableRectDiv.style.left = (rect.left + scrollLeft) + 'px';
    // We want rect.width to be the border width, so content width is 2px less.
    tableRectDiv.style.width = (rect.width - 2) + 'px';
    tableRectDiv.style.height = (rect.height - 2) + 'px';
    document.body.appendChild(tableRectDiv);
  }
}

var as = document.querySelectorAll("a");
for (var i = 0; i < as.length; i++) {
  addClientRectsOverlay(as[i]);
}
|]

runJS :: WK2.WebView -> Javascript () -> IO ()
runJS wv (Javascript js) = WK2.webViewRunJavascript wv js noCancellable Nothing
