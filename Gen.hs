{-# LANGUAGE LambdaCase #-}

module Gen where

import           Control.Arrow (first, second)
import           Control.Monad (unless)
import           Control.Monad.Trans.State.Strict
import           Data.Foldable (traverse_)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)
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

type M t s = Map s (Map (Maybe t) s)
data NFA t s = NFA s (Set s) (M t s) deriving (Show)

insertEdge s e g = Map.insertWith Map.union s $ Map.singleton e g

nfa re = (`evalState` (0, Map.empty)) $ do
  initial <- newNode
  accepting <- newNode
  go initial accepting re
  NFA initial (Set.singleton accepting) . snd <$> get
    where
      addedge s e g = modify $ second $ insertEdge s e g
      newNode = do
        cur <- fst <$> get
        modify $ first succ
        pure cur
      go s g =
        \case
          Empty -> addedge s Nothing g
          Symbol sym -> addedge s (Just sym) g
          Union r1 r2 -> do
            go s g r1
            go s g r2
          Concat r1 r2 -> do
            tmp <- newNode
            go s tmp r1
            go tmp g r2
          Many r -> do
            tmp <- newNode
            addedge s Nothing tmp
            go tmp tmp r
            addedge tmp Nothing g

closure f = go
  where
    go s = let es = f s in if es `Set.isSubsetOf` s then s else go $ Set.union es s

lookupEdge e m s = do
  edges <- Map.lookup s m
  Map.lookup e edges

followEdge e m = Set.foldl (\set s -> maybe set (`Set.insert` set) $ lookupEdge e m s) Set.empty

followSym :: Ord t => Ord s => t -> M t s -> Set s -> Set s
followSym = followEdge . Just

epclose :: Ord t => Ord s => M t s -> Set s -> Set s
epclose = closure . followEdge Nothing

eval m cur s = followSym s m $ epclose m cur

match (NFA initial accepting m) = Set.isSubsetOf accepting . epclose m . foldl (eval m) (Set.singleton initial)

setEdges :: Ord t => Ord s => M t s -> Set s -> Set (Maybe t)
setEdges m = Set.foldl (\set s -> foldr Set.insert set $ maybe [] Map.keys $ Map.lookup s m) Set.empty

setEdgeSymbols m s = Set.fromList $ catMaybes $ Set.toList $ setEdges m s

type MD t s = Map (Set s) (Map t (Set s))
data DFA t s = DFA (Set s) (Set s) (MD t s) deriving (Show)

matchDFA (DFA initial accepting dfaEdges) = go initial
  where
    go cur =
      \case
        [] -> accepting `Set.isSubsetOf` cur
        s:ss ->
          case lookupEdge s dfaEdges cur of
            Nothing -> False
            Just new -> go new ss

dfa (NFA initial accepting m) =
  DFA
  (epclose m $ Set.singleton initial)
  (epclose m accepting) $
  (`execState` Map.empty) $ go [epclose m $ Set.singleton initial]
  where
    go todo = unless (null todo) $ do
        let cur = head todo
            syms = setEdgeSymbols m cur
            pairs = map (\sym -> (sym, epclose m $ followSym sym m cur)) $ Set.toList syms
        p <- get
        let newStates = filter (not . (`Map.member` p)) $ Set.toList $ Set.fromList $ map snd pairs
        traverse_ (\(sym, s') -> modify $ insertEdge cur sym s') pairs
        go (newStates ++ tail todo)

