nano-smt
========

NanoSMT is a "toy" SMT Solver for educational purposes.

NanoSMT will combine a basic

1. Propositional Logic                (DPLL) 

with a Nelson-Oppen combination of theory solvers for 

2. Equality & Uninterpreted Functions (Congruence Closure) 
3. Difference Constraints             (Bellman-Ford)

Over time, we may add:

- Fancy SAT heuristics like non-chronological backtrack, etc.

- Linear Arithmetic via Simplex.


TODO
----

- Write SAT solver   

- Test  SAT solver -------------------- HERE

- Write EQ solver 
    - Write HashCons
    - Write UnionFind

- Test  EQ solver
- Write NO = SAT + EQ
- Test  NO solver

- Add   UIF/CC to EQ solver
- Test  EUF 
- Test  SAT+EUF 
- Write DIFF solver
- Test  DIFF solver
- Test  SAT+EUF+DIFF



-----------------------------------------------------------------
-----------------------------------------------------------------
-----------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Foo where

class Foo a where
  zz :: a -> String

data Obj = forall a. (Foo a) => Obj a

instance Show a => Foo a where
  zz = show

xs :: [Obj]
xs =  [Obj 1, Obj "foo", Obj 'c']

doShow :: [Obj] -> String
doShow [] = ""
doShow ((Obj x):xs) = zz x ++ doShow xs

bob :: [Obj] -> [String]
bob xs = map (\(Obj x) -> zz x) xs

