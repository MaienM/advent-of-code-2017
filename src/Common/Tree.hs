module Common.Tree where
import Data.List ((\\))
import Data.Tree (Tree, unfoldTree)
import GHC.Exts (the)
import qualified Data.Map

-- Build a tree from a list of objects that have an identifier of sorts + a list of identifiers for their children
-- There should be a single root object (an object that is not referred to by any other object)
buildTree :: (Eq b, Ord b) => (a -> b) -> (a -> [b]) -> [a] -> Tree a
buildTree key children items
   | (length rootKeys) == 0 = error "Tree has no root"
   | (length rootKeys) > 1 = error "Tree has multiple roots"
   | otherwise = unfoldTree (\i -> (i, map get (children i))) (get (the rootKeys))
   where
      mapped = Data.Map.fromList (map (\i -> (key i, i)) items)
      get = (Data.Map.!) mapped
      rootKeys = map key items \\ concat (map children items)
