-- | Congruence Closure For Theory of Equality and Uninterpreted Functions 

module Language.Nano.SMT.CongClos () where

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S
import Control.Monad.State
import Data.Monoid
import Data.List                     (find)
import Control.Applicative           ((<$>))

import Language.Nano.SMT.Misc
import Language.Nano.SMT.Types
import Control.Exception             (assert)

instance IsSolver CCState where
  init = initialState
  upd  = updateState 
  

-- | State for Congruence Closure Engine

type CC      = State CCState

data EqCause = EC { src :: HExpr, dst :: HExpr, cause :: HId }

data CCState = CCState {
    parent   :: M.HashMap HExpr (HExpr, EqCause)   
  , diseqs   :: S.HashSet (HExpr, HExpr, HId) 
  , elements :: M.HashMap HExpr (S.HashSet HExpr)
  }

------------------------------------------------------------------
initialState :: CCState 
------------------------------------------------------------------

-- | Initial State of Congruence Closure Solver

initialState = CCState M.empty S.empty M.empty

------------------------------------------------------------------
updateState :: [HAtom] -> CCState -> (SolveResult, CCState)
------------------------------------------------------------------

-- | Update State With New Equalities and Disequalities

updateState = runState . addAtoms

addAtoms    :: [HAtom] -> CC SolveResult

addAtoms as = do rs    <- mapM addAtom as
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
                 modify $ \st -> st { parent   = M.insert xr (yr,  EC x y i) par  }
                                    { elements = M.insert yr (S.union xs ys) elts }
                 Eqs   <$> equates xs ys

equates       :: S.HashSet HExpr -> S.HashSet HExpr -> CC [Equality] 

equates xs ys = forM xys $ \(x, y) -> equal x y <$> eqCause x y
  where 
    xys       = [(x, y) | x <- S.toList xs, y <- S.toList ys]


------------------------------------------------------------------
addNe :: HExpr -> HExpr -> HId -> CC SolveResult 
------------------------------------------------------------------

addNe x y i = do modify $ \st -> st {diseqs = S.insert (x, y, i) (diseqs st)}
                 checkContra

------------------------------------------------------------------
checkContra :: CC SolveResult 
------------------------------------------------------------------

checkContra 
  = do xyis  <- S.toList . diseqs <$> get
       rs    <- forM xyis $ \(x, y, i) -> do
                 xr <- root x
                 yr <- root y
                 if (xr == yr)
                   then (Contra . (i :)) <$> eqCause x y
                   else return mempty
       return $ mconcat rs

------------------------------------------------------------------
-- | Generating the Equality Causes ------------------------------
------------------------------------------------------------------

eqCause :: HExpr -> HExpr -> CC Cause 
eqCause x y = do r     <- ancestor x y
                 xs    <- pathCause <$> getPath x r
                 ys    <- pathCause <$> getPath y r
                 return $ xs ++ ys

linkCause x     = do Just (y, EC x' y' i) <- getParent x
                     cx    <- pathCause <$> getPath x' x
                     cy    <- pathCause <$> getPath y' y
                     return $ (i : (cx ++ cy))

-- pathCause xs    = concatMapM linkCause xs 
pathCause = undefined

------------------------------------------------------------------------
-- | Traversing the Parent Links ---------------------------------------
------------------------------------------------------------------------

root          :: HExpr -> CC HExpr
root x        = do px <- getParent x
                   maybe (return x) (root . fst) px


getParent x   = do p     <- parent <$> get
                   return $ M.lookup x p

getPath x r 
  | x == r    = return []
  | otherwise = do Just (xp,_) <- getParent x
                   return       $ x : getPath xp r
 
ancestor      :: HExpr -> HExpr -> CC HExpr 

ancestor x y  = do xr <- root x
                   yr <- root y
                   px <- getPath x xr
                   py <- getPath y yr
                   case find (`elem` py) px of
                     Just r  -> return r
                     Nothing -> return $ assert (xr == yr) xr


