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

tester m' = and $ map m' ["123", "abc", "ab34", "   "] ++ map (not . m') ["$", "_", "34a"]

testDeriv = tester $ matchDeriv token

testNFA = tester $ match $ nfa token

testDFA = tester $ matchDFA $ dfa $ nfa token

test = and [testDeriv, testNFA, testDFA]
