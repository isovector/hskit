module Data.Trie where

import qualified Data.Map as M
import           Data.Monoid
import           Prelude hiding (lookup)


data Trie k v = Trie
  { tSubtries :: M.Map k (Maybe v, Trie k v)
  }
  deriving (Functor, Show, Eq)


empty :: Trie k v
empty = Trie $ M.empty


leaf :: Ord k => k -> v -> Trie k v
leaf k v = Trie $ M.fromList [(k, (Just v, empty))]


glue :: Ord k => k -> Trie k v -> Trie k v
glue k sub = Trie $ M.fromList [(k, (Nothing, sub))]


singleton :: Ord k => [k] -> v -> Trie k v
singleton keys v = foldr glue (leaf (last keys) v) $ init keys


instance Ord k => Semigroup (Trie k v) where
  Trie s1 <> Trie s2 = Trie $
    M.unionWith (\(n1, t1) (n2, t2) ->
      (getFirst $ foldMap First [n1, n2], t1 <> t2)
                ) s1 s2

instance Ord k => Monoid (Trie k v) where
  mempty = empty


follow :: Ord k => Trie k v -> k -> (Maybe (Trie k v), Maybe v)
follow (Trie s) k =
  case M.lookup k s of
    Nothing -> (Nothing, Nothing)
    Just (v, t@(Trie s')) ->
      if M.size s' == 0
         then (Nothing, v)
         else (Just t, v)


fromList :: Ord k => [([k], v)] -> Trie k v
fromList = foldMap $ uncurry singleton


lookup :: Ord k => Trie k v -> [k] -> Maybe v
lookup _ [] = Nothing
lookup t [k] = snd $ follow t k
lookup t (k : ks) = fst (follow t k) >>= flip lookup ks


commands :: Trie Char String
commands = fromList
  [ ("j", "down")
  , ("gj", "visual down")
  , ("gu", "lower case")
  , ("gu$", "lower case to end")
  , ("gg", "top")
  ]


