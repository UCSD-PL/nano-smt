
-- | Module containing all globally visible type definitions

module Language.Nano.SMT.Types where

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

data TheoryCube a  = [(a, Atom)]

-- | Theory Formula

data TheoryFormula = TheoryCube PVar


--------------------------------------------------------------------------
-- | Source Level Formulas -----------------------------------------------
--------------------------------------------------------------------------

-- | These will only be used for parsing.

data Formula   = Atom Atom
               | PVar PropVar
               | And  [Formula]
               | Or   [Formula]
               | Not  Formula
                 deriving (Eq, Show)

--------------------------------------------------------------------------
-- | Conjunctive Normal Form Formulas ------------------------------------
--------------------------------------------------------------------------

-- | Propositional Variables

newtype PVar     = P Int
                   deriving (Eq, Ord, Show)

-- | Literals

data Literal     = Pos PropVar | Neg PropVar 
                   deriving (Eq, Ord, Show)
    
-- | Clauses 

type Clause      = [Literal]

-- | CNF Formulas

type CNF_Formula = [Clause]

--------------------------------------------------------------------------
-- | SMT Formulas --------------------------------------------------------
--------------------------------------------------------------------------

type SmtFormula = (CnfFormula, TheoryFormula) 

--------------------------------------------------------------------------
-- | Solver Results ------------------------------------------------------
--------------------------------------------------------------------------

data SatResult      = Unsat | Asgn [Literal]        deriving (Show)

data TheoryResult a = Sat   | Blocking [a]          deriving (Show)

--------------------------------------------------------------------------

-- | Returns either a satisfying assignment or `Unsat` (which could
--   carry a resolution proof...

sat_solver    :: CNF_Formula -> SAT_Result

---------------------------------------------------------------

type HId = Int

data HExpr = HVar TVar             HId 
           | HCon Int              HId 
           | HOp  Operator [HExpr] HId 
             deriving (Show)

data HAtom = HRel Relation [HExpr] HId
             deriving (Show)

instance Hashed a where
  hid :: a -> HId

instance Hashed HExpr where
  hid (HVar _   i) = i
  hid (HCon _   i) = i
  hid (HOp  _ _ i) = i

instance Hashed HAtom where
  hid (HRel _ _ i) = i

instance Hashed a => Eq a where
  x == x' = hid x == hid x'

instance Hashed a => Ord a where
  compare x x' = compare (hid x) (hid x')

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

data SolveResult = Eqs    [(Equality, Cause)] 
                 | Contra Cause

-- | Data type for representing @Equality@ between two (hash-consed) terms

newtype Equality = Eq (HExpr, HExpr) -- ^ Canonical ordering by HId 

-- | Data type to explain how an equality was deduced; required to generate
--   blocking/conflict clause (and if you so desire, a proof)

data Cause       = [HAtom]

-- | Make this instance to combine results from different theory solvers.

instance Monoid SolveResult where
  mempty                  = Eqs []
  mappend c@(Contra _) _  = c
  mappend _ c@(Contra _)  = c
  mappend (Eq xs) (Eq ys) = Eq $ sortNub $ xs ++ ys

-- | Smart constructor for creating @Equality@ values

eq x y | hid x < hid y = Eq (x, y)
       | otherwise     = Eq (y, x) 

instance Eq Equality where 
  (Eq (e1, e2)) == (Eq (e1', e2')) = (e1 == e1') && (e2 == e2') 

