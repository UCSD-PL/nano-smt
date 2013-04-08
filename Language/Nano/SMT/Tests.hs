-- | Module with candidate test Values for Each Type 

module Language.Nano.SMT.Tests (runTests, tests) where

import Test.HUnit
import Language.Nano.SMT.Types
import Language.Nano.SMT.SAT 

runTests :: IO ()
runTests = runTestTT tests >>= putStrLn . show 

---------------------------------------------------------------------------
-- Tests for SAT ----------------------------------------------------------
---------------------------------------------------------------------------

tests = test $ zipWith satTest [0..] satTests 

satTest i (b, f)    = name ~: name ~: b ~=? exec f    
  where 
    name            = "sat" ++ show i
    exec            = asBool . sat_solver  
    asBool (Asgn _) = True
    asBool _        = False

satTests :: [(Bool , CnfFormula)]
satTests 
  =  [ (False, cnf [[]])
     
     , (True , cnf [[1 , 2]
                   ,[-1, 3]
                   ,[-3   ]])
     
     , (False, cnf [[ 1]
                   ,[-1]])
     
     , (True , cnf [[-9, 1, 2]     
                   ,[ 9, 2, 3]  
                   ,[ 9, 2,-3]      
                   ,[ 9,-2, 3]        
                   ,[ 9,-2,-3]     
                   ,[-1,-2, 3]      
                   ,[-9, 1,-2]       
                   ,[-9,-1, 2]])
     
     , (True , cnf [ [1     ,   4]
                   , [1 , -3,  -8]
                   , [1 ,  8,  12]
                   , [2     ,  11]
                   , [-7, -3,   9]
                   , [-7,  8,  -9]
                   , [7 ,  8, -10]
                   , [7 , 10, -12]])
     ]


