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

data R t
  = Null
  | Empty
  | Symbol t
  | Union (R t) (R t)
  | Concat (R t) (R t)
  | Many (R t)
  deriving (Eq)

-- stolen from http://matt.might.net/articles/implementation-of-regular-expression-matching-in-scheme-with-derivatives/
deriv c =
  \case
    Null -> Null
    Empty -> Null
    Symbol c' -> if c == c' then Empty else Null
    Union r1 r2 -> deriv c r1 `Union` deriv c r2
    Concat r1 r2 -> (d r1 `Concat` deriv c r2) `Union` (deriv c r1 `Concat` r2)
      where
        d =
          \case
            Null -> Null
            Empty -> Empty
            Symbol {} -> Null
            Union r1 r2 -> d r1 `Union` d r2
            Concat r1 r2 -> d r1 `Concat` d r2
            Many {} -> Empty
    Many r -> deriv c r `Concat` Many r

nullRE =
  \case
    Null -> False
    Empty -> True
    Symbol {} -> False
    Union r1 r2 -> nullRE r1 || nullRE r2
    Concat r1 r2 -> nullRE r1 && nullRE r2
    Many {} -> True

matchDeriv r = nullRE . foldl (flip deriv) r

type M t s = Map s (Map t s)
data NFA t s = NFA (Set s) (Set s) (M (Maybe t) s) deriving (Show)

insertEdge s e g = Map.insertWith Map.union s $ Map.singleton e g

-- mostly stolen from
-- https://github.com/alanz/HaRe/blob/master/old/tools/base/parse2/LexerGen/FSM.hs
nfa re = (`evalState` (0, Map.empty)) $ do
  initial <- newNode
  accepting <- newNode
  go initial accepting re
  NFA (Set.singleton initial) (Set.singleton accepting) . snd <$> get
    where
      addedge s e g = modify $ second $ insertEdge s e g
      newNode = do
        cur <- fst <$> get
        modify $ first succ
        pure cur
      go s g =
        \case
          Null -> pure ()
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

followSym :: Ord t => Ord s => t -> M (Maybe t) s -> Set s -> Set s
followSym = followEdge . Just

epclose :: Ord t => Ord s => M (Maybe t) s -> Set s -> Set s
epclose = closure . followEdge Nothing

eval m cur s = followSym s m $ epclose m cur

match (NFA initial accepting m) = Set.isSubsetOf accepting . epclose m . foldl (eval m) initial

setEdges :: Ord t => Ord s => M t s -> Set s -> Set t
setEdges m = Set.foldl (\set s -> foldr Set.insert set $ maybe [] Map.keys $ Map.lookup s m) Set.empty

setEdgeSymbols m s = Set.fromList $ catMaybes $ Set.toList $ setEdges m s

data DFA t s = DFA (Set s) (Set s) (M t (Set s)) deriving (Show)

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
  (epclose m initial)
  (epclose m accepting) $
  (`execState` Map.empty) $ go [epclose m initial]
  where
    go todo = unless (null todo) $ do
        let cur = head todo
            syms = setEdgeSymbols m cur
            pairs = map (\sym -> (sym, epclose m $ followSym sym m cur)) $ Set.toList syms
        p <- get
        let newStates = filter (not . (`Map.member` p)) $ Set.toList $ Set.fromList $ map snd pairs
        traverse_ (\(sym, s') -> modify $ insertEdge cur sym s') pairs
        go (newStates ++ tail todo)
