-- | Nelson-Oppen Procedure for Combining Theory Solvers

module Language.Nano.SMT.NelsonOppen (theory_solver) where

-------------------------------------------------------------------------------
--
-- | returns the `k` belonging to the conflicting `Atom`
--   or empty list if the conjunction of `[Atom]` is satisfiable 

theory_solver     :: TheoryCube k -> [k] 
theory_solver     = undefined

-- data NelsonOppenState = NOST {
--     exprs      :: HashMap HId HExpr
--   , atoms      :: HashMap HId HAtom
--   , equalities :: HashSet (HId, HId)
--   , causes     :: HashMap HId [HId]
--   , solvers    :: [Solver] 
--   }
-- theory_solver kas = runState (theory_solver' kas) st0 
--   where 
--     (ks, as)      = unzip katoms
--     st0           = NOST M.empty M.empty S.empty M.empty ss
--     ss            = [CongClos.solver, DiffBound.solver]  
-- 
-- theory_solver' ks atoms 
--   = do hatoms  <- mapM hashConsAtom atoms
--        let km   = M.fromList $ zip (hid <$> hatoms) ks
--        res     <- crunch
--        let core = resultHAtoms res
--        return     [ km ! hid a | a <- core ]
-- 
-- resultAtoms :: Maybe Cause -> NO [HAtom]
-- resultAtoms = undefined
-- 
-- -- | crunch atoms == Nothing means atoms is SAT, Just c means CONTRA
-- 
-- crunch :: [HAtoms] -> NO (Maybe Cause) 
-- crunch [] 
--   = return Nothing 
-- crunch eqs 
--   = do ss <- solvers  <$> get
--        case crunchSolvers ss eqs of
--          (_  , Contra c) -> return (Just c) 
--          (ss', Eqs xs  ) -> updSolvers ss' >> updEqualities xs >>= crunch 
-- 
-- 
-- updSolvers    :: [Solver] -> NO () 
-- updSolvers ss = modify $ \st -> st {solvers = ss } 
-- 
-- updEqualities :: [(Equality, Cause)] -> NO [HAtoms]
-- updEqualities = undefined 
--   -- known  <- equalities <$> get
--   -- let xs' = filter (not . known) xs
--   -- add xs' to equalities
-- 
-- crunchSolvers :: [Solver] -> [HAtom] -> ([Solver], SolveResult) 
-- crunchSolvers =  undefined  
-- 
-- crunchSolver  :: Solver -> [HAtom] -> (Solver, SolveResult)
-- crunchSolver  =  undefined
-- 
-- hashConsAtom  :: Atom -> NO HAtom
-- hashConsAtom  =  undefined
-- 
-- hashConsExpr  :: Expr -> NO HExpr
-- hashConsExpr  =  undefined
-- 
