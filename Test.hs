module Test where

import Gen

-- test
range start end = foldr1 Union $ map Symbol [start .. end]
digit = range '0' '1'
digits = digit `Concat` Many digit

alpha = range 'a' 'b' `Union` range 'A' 'B'
alphanum = alpha `Union` digit

iden = alpha `Concat` Many alphanum

ws = Many $ Symbol ' '

token = Many $ foldr1 Union [digits, iden, ws]
