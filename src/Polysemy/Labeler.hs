{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -Wall   #-}

module Polysemy.Labeler where

import           Control.Monad.IO.Class
import           Data.Bifunctor
import           Data.Foldable
import           Data.GI.Base
import           Data.Traversable
import           Foreign.C.Types
import           GHC.OverloadedLabels
import qualified GI.WebKit2WebExtension as WE
import           Polysemy
import           Polysemy.NonDet
import           Polysemy.Operators
import qualified Data.Text as T

data AABB a = AABB
  { aMin :: (a, a)
  , aMax :: (a, a)
  } deriving (Eq, Ord, Show, Read, Functor)


aabbsIntersect :: Ord a => AABB a -> AABB a -> Bool
aabbsIntersect (AABB (al, au) (ar, ad))
               (AABB (bl, bu) (br, bd)) =
  and [ ar >= bl
      , al <= br
      , ad >= bu
      , au <= bd
      ]


data Labeler m a where
  GetLinkRects    :: Labeler m [AABB Float]
  SetLabels   :: [(String, AABB Float)] -> Labeler m ()

makeSem ''Labeler

type IsContainer m a b =
  ( MonadIO m
  , IsLabel "getLength" (a -> m CULong)
  , IsLabel "item" (a -> CULong -> m b)
  )

itemsOf
  :: IsContainer m a b
  => m a
  -> m [b]
itemsOf mctr = do
  ctr <- mctr
  (len :: CULong) <- #getLength ctr
  for [0 .. len - 1] $ \i ->
    #item ctr i


runLabelerWebExt :: WE.DOMDOMWindow -> Labeler :r@> a -> '[Lift IO, NonDet] >@r@> a
runLabelerWebExt win = interpret \case
  GetLinkRects -> do
    sx <- fromIntegral <$> #getScrollX win
    sy <- fromIntegral <$> #getScrollY win

    doc <- #getDocument win
    nodes <- itemsOf $ #querySelectorAll doc "a"
    for nodes $ \node -> do
      Just e <- sendM $ castTo WE.DOMElement node
      rect <- #getBoundingClientRect e
      tl <- (,) <$> #getLeft rect  <*> #getTop rect
      br <- (,) <$> #getRight rect <*> #getBottom rect
      pure $ AABB (bimap (+sx) (+sy) tl)
                  (bimap (+sx) (+sy) br)

  SetLabels ls -> do
    doc <- #getDocument win

    old_els <- itemsOf $ #querySelectorAll doc ".__hskit_label__"
    for_ old_els $ #removeChild doc

    for_ ls $ \(label, AABB (rx, ry) (rx', ry')) -> do
      el <- #createElement doc "div"
      #setInnerHtml el $ T.pack label
      #setClassName el "__hskit_label__"
      style <- #getStyle el
      #setProperty style "position" "absolute" ""
      #setProperty style "left" (T.pack $ show rx) ""
      #setProperty style "top" (T.pack $ show ry) ""
      #setProperty style "width" (T.pack $ show $ rx' - rx) ""
      #setProperty style "height" (T.pack $ show $ ry' - ry) ""
      #setProperty style "border" "1px solid #f00" ""

