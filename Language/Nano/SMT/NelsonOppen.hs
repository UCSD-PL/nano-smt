-- | Nelson-Oppen Procedure for Combining Theory Solvers

module Language.Nano.SMT.NelsonOppen (theory_solver) where

import Language.Nano.SMT.Types
import Control.Monad.State
import qualified Data.HashMap.Strict           as M
import qualified Data.HashSet                  as S
import Control.Applicative           ((<$>))
import qualified Language.Nano.SMT.CongClos    as CC

-------------------------------------------------------------------------------
-- | `theory_solver` returns the `k` belonging to the conflicting `Atom`
--   or empty list if the conjunction of `[Atom]` is satisfiable 

theory_solver     :: TheoryCube k -> [k] 

theory_solver kas = runState (theory_solver' kas) st0 
  where 
    (ks, as)      = unzip kas
    st0           = NOST M.empty M.empty S.empty M.empty solvers 0

theory_solver' ks atoms 
  = do hatoms    <- mapM hashConsAtom atoms
       let km     = M.fromList $ zip (hid <$> hatoms) ks
       res       <- crunch
       core      <- resultHAtoms res
       return     $ [km ! hid a | a <- core]

solvers = [SS cc0] --  DiffBound.solver  
  where 
    cc0 = init :: CCState

------------------------------------------------------------------------------
-- | Transitively grind causes down into [HAtom] -----------------------------
------------------------------------------------------------------------------

resultHAtoms :: Maybe Cause -> NO [HAtom]
resultHAtoms Nothing   = return []
resultHAtoms (Just is) = ((`grindCauses` is) . causes) <$> get

grindCauses :: M.HashMap HId [HId] -> [HId] -> [HId]
grindCauses cm = sortNub . goMany
  where 
    go i       = maybe [] goMany (M.lookup i cm) 
    goMany     = concatMap go

------------------------------------------------------------------------------
-- | State for Nelson-Oppen Engine -------------------------------------------
------------------------------------------------------------------------------

data NelsonOppenState = NOST {
    exprs      :: HashMap HId HExpr
  , atoms      :: HashMap HId HAtom
  , equalities :: HashSet (HId, HId)
  , causes     :: HashMap HId [HId]
  , solvers    :: [Solver]
  , counter    :: Int
  }

-------------------------------------------------------------------------------
-- | crunch atoms == Nothing means atoms is SAT, Just c means CONTRA
-------------------------------------------------------------------------------

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

crunchSolvers            :: [Solver] -> [HAtom] -> ([Solver], SolveResult) 
crunchSolvers solvers as = (solvers', mconcat result) 
  where 
    (solvers', results)  = unzip $ map (`crunchSolver` as) solvers 

crunchSolver  :: Solver -> [HAtom] -> (Solver, SolveResult)
crunchSolver  (SS state) as = (SS state', result) 
  where 
    (result, state')        = upd as state 

------------------------------------------------------------------------------
-- | Registering New Equalities ----------------------------------------------
------------------------------------------------------------------------------

updEqualities :: [Equality] -> NO [HAtom]
updEqualities eqs
  = do known   <- equalities <$> get
       let eqs' = [e | e <- eqs, (lhs e, rhs e) `S.notElem` known] 
       mapM addEq eqs'

addEq :: (Eq x y c) -> NO HAtom
addEq (Eq x y c) 
  = do a     <- hashConsAtom $ Rel Eq [x, y]
       modify $ \st -> { causes     = H.insert (hid a) c (causes st)  }
                       { equalities = S.insert (x, y) (equalities st) }
       return a

--------------------------------------------------------------------
-- | Hash Consing: Building the DAG --------------------------------
--------------------------------------------------------------------

fresh :: NO Int
fresh = do st    <- get
           let n  = counter st
           put    $ st { counter = n + 1 }
           return n 

--------------------------------------------------------------------

hashConsAtom  :: Atom -> NO HAtom
hashConsAtom  =  undefined
 
hashConsExpr  :: Expr -> NO HExpr
hashConsExpr  =  undefined

--------------------------------------------------------------------

