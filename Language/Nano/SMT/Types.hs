
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

-- | returns the `k` belonging to the conflicting `Atom`
--   or empty list if the conjunction of `[Atom]` is satisfiable 

theory_solver :: [(k, Atom)] -> [k] 

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


hashConsExpr   :: Expr -> NO HExpr
hashConsAtom   :: Atom -> NO HAtom

data NelsonOppenState = {
    exprs      :: HashMap HId HExpr
  , atoms      :: HashMap HId HAtom
  , equalities :: HashSet (HId, HId)
  , causes     :: HashMap HId [HId]
  , solvers    :: [Solver] 
  }

theory_solver katoms = runState (theory_solver' katoms) st0 
  where 
    (ks, atoms)      = unzip katoms


theory_solver' ks atoms 
  = do hatoms  <- mapM hashConsAtom atoms
       let km   = M.fromList $ zip (hid <$> hatoms) ks
       res     <- crunch
       let core = resultHAtoms res
       return     [ km ! hid a | a <- core ]

resultAtoms :: Maybe Cause -> NO [HAtom]
resultAtoms = undefined

-- | crunch atoms == Nothing means atoms is SAT, Just c means CONTRA

crunch :: [HAtoms] -> NO (Maybe Cause) 
crunch [] 
  = return Nothing 
crunch eqs 
  = do ss <- solvers  <$> get
       case crunchSolvers ss eqs of
         (_  , Contra c) -> return (Just c) 
         (ss', Eqs xs  ) -> updSolvers ss' >> updEqualities xs >>= crunch 


updSolvers    :: [Solver] -> NO () 
updSolvers ss = modify $ \st -> st {solvers = ss } 

updEqualities :: [(Equality, Cause)] -> NO [HAtoms]
updEqualities = undefined 
  -- known  <- equalities <$> get
  -- let xs' = filter (not . known) xs
  -- add xs' to equalities

createSolvers :: [HAtom] -> ([Solver], SolveResult)
createSolvers = undefined

crunchSolvers :: [Solver] -> [HAtom] -> ([Solver], SolveResult) 
crunchSolvers = undefined  

----------------------------------------------------------------------

newtype Equality = Eq (HExpr, HExpr) -- ^ Canonical ordering by HId 

-- | Only expose the "smart" constructor

eq x y | hid x < hid y = Eq (x, y)
       | otherwise     = Eq (y, x) 

instance Eq Equality where 
  (Eq (e1, e2)) == (Eq (e1', e2')) = (e1 == e1') && (e2 == e2') 

----------------------------------------------------------------------

data Cause       = [HAtom]

data SolveResult = Eqs    [(Equality, Cause)] 
                 | Contra Cause

assertEqual      :: (HExpr, HExpr) -> [HAtom] -> NO ()
assertContra     :: 

class IsSolver st where
  init :: st
  upd  :: [HAtom] -> st -> (SolveResult, st)

data Solver = forall st. (IsSolver st) => SS st

instance Monoid SolveResult where
  mempty                  = Eqs []
  mappend c@(Contra _) _  = c
  mappend _ c@(Contra _)  = c
  mappend (Eq xs) (Eq ys) = Eq $ sortNub $ xs ++ ys

crunchSolver :: Solver -> [HAtom] -> (Solver, SolveResult)


  
-----------------------------------------------------------------

module NelsonOppen where

-- NOW WRITE NELSON OPPEN, given 
-- (Solver a) => [a]



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



--------------------------------------------------------------
