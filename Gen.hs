{-# LANGUAGE LambdaCase #-}
import qualified Data.Map.Strict as Map
import Control.Monad.Trans.State.Strict

-- mostly stolen from
-- https://github.com/alanz/HaRe/blob/master/old/tools/base/parse2/LexerGen/FSM.hs

data R t
  = Empty
  | Symbol t
  | Union (R t) (R t)
  | Concat (R t) (R t)
  | Many (R t)

data Edge t = Epsilon | T t deriving (Eq,Ord,Show)
type M t s = Map.Map s (Map.Map (Edge t) s)
newtype NFA t s = NFA (M t s) deriving (Show)

addedge :: Ord s => Ord t => s -> Edge t -> s -> State (s, M t s) ()
addedge s e g = do
  (cur, edges) <- get
  let edges' = Map.insertWith Map.union s (Map.singleton e g) edges
  put (cur, edges')

newNode :: Enum s => State (s, M t s) s
newNode = do
  (cur, edges) <- get
  put (succ cur, edges)
  pure cur

compile :: Enum s => Ord s => Ord t => R t -> State (s, M t s) ()
compile re = do
  n1 <- newNode
  n2 <- newNode
  go n1 n2 re
    where
      go s g =
        \case
          Empty -> addedge s Epsilon g
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
            addedge s Epsilon tmp
            go tmp tmp r
            addedge tmp Epsilon g

comp re = snd (compile re `execState` (0, Map.empty))

-- test
range start end = foldr1 Union $ map Symbol [start .. end]
digit = range '0' '9'
digits = digit `Concat` Many digit

alpha = range 'a' 'z' `Union` range 'A' 'Z'
alphanum = alpha `Union` digit

iden = alpha `Concat` (Many alphanum)

ws = Many $ Symbol ' '

token = Many $ foldr1 Union [digits, iden, ws]
