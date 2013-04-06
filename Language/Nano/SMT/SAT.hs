-- | DPLL-based SAT solver, adapted from https://github.com/gatlin/surely

module Language.Nano.SMT.SAT (sat_solver) where

-- | Returns either a satisfying assignment or `Unsat` (which could carry a resolution proof...)

sat_solver :: CNF_Formula -> SAT_Result
sat_solver = undefined 

