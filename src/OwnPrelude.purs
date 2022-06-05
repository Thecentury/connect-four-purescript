module OwnPrelude where

import Prelude

import Control.Monad.Reader (ReaderT, mapReaderT)
import Data.Identity (Identity(..))
import Data.List (List(..), drop)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import Effect.Exception.Unsafe (unsafeThrow)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldr)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Node.Encoding as Encoding
import Node.Process as Process
import Node.ReadLine (Interface)
import Node.ReadLine as ReadLine
import Node.Stream (writeString)

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

liftReader :: forall m r b . Monad m => ReaderT r Identity b -> ReaderT r m b
liftReader = mapReaderT (\(Identity v) -> pure v)

--------------------------------------------------------------------------------

question :: String -> Interface -> Aff String
question message interface = makeAff go
  where
    go runAffFunction = nonCanceler <$
      ReadLine.question message (runAffFunction <<< Right) interface

putStr :: String -> Aff Unit
putStr s = do
  _ <- liftEffect $ writeString Process.stdout Encoding.ASCII s (\_ -> pure unit)
  pure unit

putStrLn :: String -> Aff Unit
putStrLn s = putStr (s <> "\n")

--------------------------------------------------------------------------------

mapM_ :: forall a b t m. Monad m => Foldable t => (a -> m b) -> t a -> m Unit
mapM_ f = foldr c (pure unit)
  where c x k = f x *> k

forM_ :: forall a b t m. Monad m => Foldable t => t a -> (a -> m b) -> m Unit
forM_ = flip mapM_