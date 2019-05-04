{-# LANGUAGE LambdaCase #-}

module Gen where

import           Control.Arrow (first, second)
import           Control.Monad.Trans.State.Strict
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
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

insertEdge s e g = Map.insertWith Map.union s $ Map.singleton e g

compile re = (`evalState` (0, Map.empty)) $ do
  nstart <- newNode
  nend <- newNode
  go nstart nend re
  NFA nstart nend . snd <$> get
    where
      addedge s e g = modify $ second $ insertEdge s e g
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

lookupEdge e m s = do
  edges <- Map.lookup s m
  Map.lookup e edges

followEdge e m = Set.foldl (\set s -> maybe set (`Set.insert` set) $ lookupEdge e m s) Set.empty

followSym :: Ord t => Ord s => t -> M t s -> Set s -> Set s
followSym = followEdge . T

epclose :: Ord t => Ord s => M t s -> Set s -> Set s
epclose = closure . followEdge E

eval m cur s = followSym s m $ epclose m cur

match (NFA start end m) = Set.member end . epclose m . foldl (eval m) (Set.singleton start)
