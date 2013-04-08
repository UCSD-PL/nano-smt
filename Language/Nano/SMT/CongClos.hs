-- | Congruence Closure For Theory of Equality and Uninterpreted Functions 

module Language.Nano.SMT.CongClos () where

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S

instance IsSolver CCState where
  init = initialState
  upd  = updateState 
  

-- | State for Congruence Closure Engine

type CC      = State CCState

data EqCause = EC { src :: HExpr, dst :: HExpr, cause :: HId }

data CCState = CCState {
    parent   :: M.Map HExpr (HExpr, EqCause)   
  , diseqs   :: S.Set (HExpr, HExpr, HId) 
  , elements :: M.Map HExpr (S.Set HExpr)
  }

------------------------------------------------------------------
initialState :: CCState 
------------------------------------------------------------------

-- | Initial State of Congruence Closure Solver

initialState = CCState M.empty S.empty

------------------------------------------------------------------
updateState :: [HAtom] -> CCState -> (SolveResult, CCState)
------------------------------------------------------------------

-- | Update State With New Equalities and Disequalities

updateState = runState . addAtoms

addAtoms    :: [HAtom] -> CC SolveResult
addAtoms as = do rs    <- mapM addAtom atoms
                 r     <- checkContra
                 return $ mconcat $ r : rs

------------------------------------------------------------------
addAtom    :: HAtom -> CC SolveResult
------------------------------------------------------------------

addAtom (HRel Eq [x, y] i) = addEq x y i 
addAtom (HRel Ne [x, y] i) = addNe x y i 
addAtom _                  = return mempty 

------------------------------------------------------------------
addEq       :: HExpr -> HExpr -> HId -> CC SolveResult 
------------------------------------------------------------------

addEq x y i = do xr    <- root x
                 yr    <- root y
                 par   <- parent   <$> get
                 elts  <- elements <$> get
                 let xs = M.lookupDefault (S.singleton xr) xr elts
                 let ys = M.lookupDefault (S.singleton xr) xr elts
                 put    $ st { parent   = M.add xr (yr, EC x y i)  par  }
                             { elements = M.add yr (S.union xs ys) elts }
                 Eqs   <$> equates xs ys

equates       :: S.Set HExpr -> S.Set HExpr -> CC [Equality] 

equates xs ys = forM xys $ \(x, y) -> equal x y <$> eqCause x y
  where 
    xys       = [(x, y) | x <- S.toList xs, y <- S.toList ys]


------------------------------------------------------------------
addNe :: HExpr -> HExpr -> HId -> CC SolveResult 
------------------------------------------------------------------

addNe x y i = modify $ \st -> st {diseqs = S.add (x, y, i) (diseqs st)}

------------------------------------------------------------------
checkContra :: CC SolveResult 
------------------------------------------------------------------

checkContra 
  = do xyis <- (S.toList . diseqs) <$> get
       rs   <- mapM checkContra' xyis 
       return $ mconcat rs
                 
checkContra' (x,y,i) 
  = do xr <- root x
       yr <- root y
       if (xr == yr)
         then (Contra . (i :)) <$> eqCause x y
         else return mempty


------------------------------------------------------------------
eqCause :: HExpr -> HExpr -> CC Cause 
------------------------------------------------------------------
eqCause = undefined

-- eqCause x = do (_,cx) <- rootCause x
--                (_,cy) <- rootCause y
--                return  $ cx ++ cy

-- proofLink : {v: HExpr | v hasParent } -> CC Cause 
proofLink x = do (y, c) <- link
    
------------------------------------------------------------------
root    :: HExpr -> CC HExpr
------------------------------------------------------------------
root x = fst <$> rootCause x


------------------------------------------------------------------
rootCause :: HExpr -> CC HExpr
------------------------------------------------------------------
rootCause x 
  = do p <- parent <$> get
       case M.lookup x p of 
         Just (x', c) -> do (x', is) <- rootCause x'
                            return (x', cause c : is)
         Nothing      -> return (x, [])



