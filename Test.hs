module Test where

import Gen

-- test
range start end = foldr1 Union $ map Symbol [start .. end]
digit = range '0' '9'
digits = digit `Concat` Many digit

alpha = range 'a' 'z' `Union` range 'A' 'Z'
alphanum = alpha `Union` digit

iden = alpha `Concat` Many alphanum

ws = Many $ Symbol ' '

token = foldr1 Union [digits, iden, ws]

testNFA =
  let m' = match $ nfa token in
  map m' ["123", "abc", "ab34", "   "] ++ map (not . m') ["$", "_", "34a"]

testDFA =
  let m' = matchDFA $ dfa $ nfa token in
  map m' ["123", "abc", "ab34", "   "] ++ map (not . m') ["$", "_", "34a"]
