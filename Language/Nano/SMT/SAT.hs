-- | DPLL-based SAT solver, adapted from https://github.com/gatlin/surely

module Language.Nano.SMT.SAT (sat_solver) where

import Language.Nano.SMT.Types
import Data.Maybe
import Control.Applicative      ((<$>))

-- | The top-level sat_solver API wrapping `dpll`.
--   Returns either a satisfying assignment `Asgn lits` 
--   or `Unsat`, which could carry a resolution proof...

sat_solver   :: CnfFormula -> SatResult
sat_solver f = maybe Unsat Asgn $! dpll $! SolverState f [] 

{-# INLINE sat_solver #-}

----------------------------------------------------------------------------------------
-- Solver State ------------------------------------------------------------------------
----------------------------------------------------------------------------------------

-- | The state of a solver at any given time is a subset of 
--   the original formula and the list of literals considered true.

data SolverState = SolverState { 
    formula      :: !CnfFormula  -- ^ Subset of original formula with 
                                 --   still "unsat" clauses and literals.
  , assignment   :: !Assignment  -- ^ Subset of literals set to "true" 
                                 --   in current partial assignment.     
  } deriving (Show)


-- | DPLL algorithm: back-tracking search with unit-propagation 

----------------------------------------------------------------------------------------
dpll :: SolverState -> Maybe Assignment 
----------------------------------------------------------------------------------------

dpll !ss  
  | solved f  = Just $ assignment ss 
  | contra f  = Nothing 
  | otherwise = first [dpll $! setLiteral l ss | l <- [lit, neg lit]]
  where 
    f         = formula ss 
    lit       = chooseLiteral f
    first     = listToMaybe . catMaybes   

-- | `setLiteral` updates the assignment to set the literal to true, 
--    and then performs `unitPropagate` 

setLiteral      :: Literal -> SolverState -> SolverState 
setLiteral l st = unitPropagate $! SolverState (simplify l f) (l : lits) 
  where
    lits        = assignment st
    f           = formula    st

{-# INLINE setLiteral #-}

-- | Returns the first `Literal` that satisfies the first `Clause` 
--   Should only be called on non-contra, non-solved CnfFormulas

chooseLiteral :: CnfFormula -> Literal
chooseLiteral = head . head

{-# INLINE chooseLiteral #-}

-- | Is the formula solved? (i.e. are there any remaining clauses)

solved :: CnfFormula -> Bool
solved = null

{-# INLINE solved #-}

-- | Is the formula a contradiction? (i.e. are there any empty clauses?)

contra :: CnfFormula -> Bool
contra = any null 

{-# INLINE contra #-}

-----------------------------------------------------------------------------
-- Unit Propagation ---------------------------------------------------------
-----------------------------------------------------------------------------

-- | `unitPropagate` simplifies formula for every variable in a unit clause
-- i.e. a clause with a single literal.

unitPropagate :: SolverState -> SolverState

unitPropagate (SolverState f r) =
    case getUnit f of
      Nothing -> SolverState f r
      Just u  -> unitPropagate $! SolverState (simplify u f) (u:r)

-- | `getUnit` returns literal for first unit clause in the formula, if one exists.

getUnit     :: CnfFormula -> Maybe Literal
getUnit !xs = listToMaybe [ x | [x] <- xs ]

-- | `simplify f l` removes clauses in f where `l` appears (they are satisfied)
-- , removes literal `not l` from clauses which contain it (they cannot be satisfied)
-- , returning a formula without any occurrences of `l`

simplify           :: Literal -> CnfFormula -> CnfFormula
simplify !l        = simpLits l . simpClaus l 
  where 
    simpLits  l cs = filter (/= neg l) <$> cs
    simpClaus l cs = filter (l `notElem`)  cs

----------------------------------------------------------------------------
-- liquidhaskell specifications --------------------------------------------
----------------------------------------------------------------------------

{-@ simplify :: f:CnfFormula 
             -> l:Literal 
             -> {v : CnfFormula  | (Subset (pvars v) (pvars f)) && (Disjoint (pvars v) (pvars l)) }
  @-}


{-@ data SolverState = SolverState { 
      formula      :: !CnfFormula  
    , assignment   :: {v: Assignment | (Disjoint (pvars formula) (pvars v)) } 
    } 
  @-}

{-@ type Assignment = [Literal]<{\x y -> pvar x /= pvar y}> @-}

{-@ type NonNull a  = {v: [a] | (len v) > 0 }               @-}
{-@ chooseLiteral :: NonNull (NonNull Literal) -> Literal   @-}
