
-- | Module containing all globally visible type definitions

module Language.Nano.SMT.Types where

import Data.Monoid
import Data.Function            (on)
import Language.Nano.SMT.Misc   (sortNub)

--------------------------------------------------------------------------
-- | Theory Entities -----------------------------------------------------
--------------------------------------------------------------------------

-- | Theory Variables

newtype TVar   = S String 
               deriving (Eq, Show)

-- | Theory Operators

data Operator  = Plus               -- ^ addition 
               | Minus              -- ^ subtraction
               | App TVar           -- ^ uninterpreted function
               deriving (Eq, Show)

-- | Relations

data Relation  = Eq                 -- ^ Equality
               | Ne                 -- ^ Disequality
               | LeC Int            -- ^ `LeC n x y` is `x - y <= n`  
               deriving (Eq, Show)

-- | Theory Expressions

data Expr      = Var TVar 
               | Con Int
               | Op  Operator [Expr]
                 deriving (Eq, Show)

-- | Theory Relations

data Atom      = Rel Relation [Expr]
                 deriving (Eq, Show)

-- | Theory Cubes

type TheoryCube a  = [(a, Atom)]

-- | Theory Formula

data TheoryFormula = TheoryCube PVar

--------------------------------------------------------------------------
-- | Source Level Formulas and Results -----------------------------------
--------------------------------------------------------------------------

-- | These will only be used for parsing.

data Formula   = Atom Atom
               | Prop PVar
               | And  [Formula]
               | Or   [Formula]
               | Not  Formula
                 deriving (Eq, Show)

-- | Output Result

data Result    = Satisfiable 
               | Unsatisfiable
                 deriving (Eq, Show)

--------------------------------------------------------------------------
-- | Conjunctive Normal Form Formulas ------------------------------------
--------------------------------------------------------------------------

-- | Propositional Variables

newtype PVar     = P Int
                   deriving (Eq, Ord, Show)

-- | Literals

data Literal     = Pos PVar | Neg PVar 
                   deriving (Eq, Ord, Show)
    
-- | Clauses 

type Clause      = [Literal]

-- | CNF Formulas

type CnfFormula = [Clause]

--------------------------------------------------------------------------
-- | SMT Formulas --------------------------------------------------------
--------------------------------------------------------------------------

type SmtFormula = (CnfFormula, TheoryFormula) 

--------------------------------------------------------------------------
-- | Solver Results ------------------------------------------------------
--------------------------------------------------------------------------

type Assignment     = [Literal]

data SatResult      = Unsat | Asgn Assignment       deriving (Show)

data TheoryResult a = Sat   | Blocking [a]          deriving (Show)

--------------------------------------------------------------------------
-- | Hash-Consed Formulas Shared Across Theory Solvers and NO ------------
--------------------------------------------------------------------------

type HId = Int

data HExpr = HVar TVar             HId 
           | HCon Int              HId 
           | HOp  Operator [HExpr] HId 
             deriving (Show)

data HAtom = HRel Relation [HExpr] HId
             deriving (Show)

class Hashed a where
  hid :: a -> HId

instance Hashed HExpr where
  hid (HVar _   i) = i
  hid (HCon _   i) = i
  hid (HOp  _ _ i) = i

instance Hashed HAtom where
  hid (HRel _ _ i) = i


hashEqual x x'   = hid x == hid x'
hashCompare x x' = compare (hid x) (hid x')

instance Eq HExpr where
  (==) = hashEqual

instance Eq HAtom where
  (==) = hashEqual
  
instance Ord HExpr where
  compare = hashCompare

instance Ord HAtom where
  compare = hashCompare

----------------------------------------------------------------------
-- | API Behavior of a Single Theory Solver --------------------------
----------------------------------------------------------------------

class IsSolver st where
  init :: st
  upd  :: [HAtom] -> st -> (SolveResult, st)

data Solver = forall st. (IsSolver st) => SS st

-------------------------------------------------------------------
-- | Data types to describe behavior of Theory Solvers ------------
-------------------------------------------------------------------

-- | Theory solver either returns (all) new discovered equalities,
--   or a contradiction if one was found (together with the cause).

data SolveResult = Eqs    [Equality] 
                 | Contra Cause

-- | Data type for representing @Equality@ between two (hash-consed) terms

data Equality = Equal { lhs   :: HExpr -- ^ Canonical ordering of lhs/rhs
                      , rhs   :: HExpr -- ^ {v: HExpr | (hid lhs) <= (hid v)}
                      , cause :: Cause -- ^ Explanation of equality
                      }

-- | Smart constructor for creating @Equality@ values

equal x y c 
  | hid x < hid y = Equal x y c
  | otherwise     = Equal y x c 

-- | Data type to explain how an equality was deduced; required to generate
--   blocking/conflict clause (and if you so desire, a proof)

type Cause       = [HAtom]

-- | Make this instance to combine results from different theory solvers.

instance Monoid SolveResult where
  mempty                    = Eqs []
  mappend c@(Contra _) _    = c
  mappend _ c@(Contra _)    = c
  mappend (Eqs xs) (Eqs ys) = Eqs $ sortNub $ xs ++ ys

instance Eq Equality where 
  (==)    = (==) `on` eqSig

instance Ord Equality where 
  compare = compare `on` eqSig

eqSig e = (hid $ lhs e, hid $ rhs e)

------------------------------------------------------------------------------------
-- Candidate Test Values for Each Type ---------------------------------------------
------------------------------------------------------------------------------------


