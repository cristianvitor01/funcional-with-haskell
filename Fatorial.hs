module Fatorial where

fatorial:: Integer -> Integer
fatorial x = if x > 0 then x * fatorial (x - 1) else 1
