module OwnPrelude where

import Prelude

import Data.List (List(..), drop)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import Effect.Exception.Unsafe (unsafeThrow)

safeSkip :: forall a . Int -> List a -> List a
safeSkip count _ | count < 0 = Nil
safeSkip 0 list = list
safeSkip toSkip list = drop toSkip list

type Zipper a = {
    left :: List a,
    focus :: a,
    right :: List a
}

zipperFromList :: forall a . List a -> Zipper a
zipperFromList Nil = unsafeThrow "zipperFromList: empty list"
zipperFromList (Cons x xs) = { left: Nil, focus: x, right: xs }

zipperWithFocus :: forall a . a -> Zipper a -> Zipper a
zipperWithFocus f z = { left: z.left, focus: f, right: z.right }

zipperTryMoveRight :: forall a . Zipper a -> Maybe (Zipper a)
zipperTryMoveRight { left, focus, right } = case right of
    Nil -> Nothing
    Cons r rs -> Just { left: Cons focus left, focus: r, right: rs }

zipperSelfAndRights :: forall a . Zipper a -> List (Zipper a)
zipperSelfAndRights zipper = unfoldr gen (Just zipper)
  where
    gen :: Maybe (Zipper a) -> Maybe (Tuple (Zipper a) (Maybe (Zipper a)))
    gen Nothing = Nothing
    gen (Just z) = Just $ Tuple z (zipperTryMoveRight z)

zipperToList :: forall a . Zipper a -> List a
zipperToList { left, focus, right } = 
    List.concat $ List.fromFoldable [List.reverse left, Cons focus right]

--------------------------------------------------------------------------------

data Tree a = Tree a (List (Tree a))

treeValue :: forall a . Tree a -> a
treeValue (Tree v _) = v

treeChildren :: forall a . Tree a -> List (Tree a)
treeChildren (Tree _ cs) = cs

--------------------------------------------------------------------------------