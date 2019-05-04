{-# LANGUAGE LambdaCase #-}

module Gen where

import qualified Data.Map.Strict as Map
import Control.Monad.Trans.State.Strict
import Control.Arrow (first, second)
-- mostly stolen from
-- https://github.com/alanz/HaRe/blob/master/old/tools/base/parse2/LexerGen/FSM.hs

data R t
  = Empty
  | Symbol t
  | Union (R t) (R t)
  | Concat (R t) (R t)
  | Many (R t)

data Edge t = E | T t deriving (Eq,Ord,Show)
type M t s = Map s (Map (Edge t) s)
data NFA t s = NFA { start:: s, end:: s, edges:: M t s } deriving (Show)

compile re = (`evalState` (0, Map.empty)) $ do
  nstart <- newNode
  nend <- newNode
  go nstart nend re
  NFA nstart nend . snd <$> get
    where
      addedge s e g = modify $ second $ Map.insertWith Map.union s $ Map.singleton e g
      newNode = do
        cur <- fst <$> get
        modify $ first succ
        pure cur
      go s g =
        \case
          Empty -> addedge s E g
          Symbol sym -> addedge s (T sym) g
          Union r1 r2 -> do
            go s g r1
            go s g r2
          Concat r1 r2 -> do
            tmp <- newNode
            go s tmp r1
            go tmp g r2
          Many r -> do
            tmp <- newNode
            addedge s E tmp
            go tmp tmp r
            addedge tmp E g

closure f = go
  where
    go s = let es = f s in if es `Set.isSubsetOf` s then s else go $ Set.union es s

followEdge :: Ord t => Ord s => Edge t -> M t s -> Set s -> Set s
followEdge e m =
  Set.unions . fmap (\s -> maybe Set.empty (maybe Set.empty Set.singleton . Map.lookup e) $ Map.lookup s m) . Set.toList

followSym :: Ord t => Ord s => t -> M t s -> Set s -> Set s
followSym = followEdge . T

epclose :: Ord t => Ord s => M t s -> Set s -> Set s
epclose = closure . followEdge E

eval m cur s = followSym s m $ epclose m cur

match (NFA start end m) s = Set.member end $ epclose m $ foldl (eval m) (Set.singleton start) s
