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
  =  [ (False, [[]])
     , (True , [[Pos 1, Pos 2]
               ,[Neg 1, Pos 3]
               ,[Neg 3]      ])
     , (False, [[Pos 1]
               ,[Neg 1]])
     , (True , [[Neg 9 , Pos 1 , Pos 2]     
               ,[Pos 9 , Pos 2 , Pos 3]  
               ,[Pos 9 , Pos 2 , Neg 3]      
               ,[Pos 9 , Neg 2 , Pos 3]        
               ,[Pos 9 , Neg 2 , Neg 3]     
               ,[Neg 1 , Neg 2 , Pos 3]      
               ,[Neg 9 , Pos 1 , Neg 2]       
               ,[Neg 9 , Neg 1 , Pos 2]])
     , (True , cnf [ [1     ,   4]
                   , [1 , -3,  -8]
                   , [1 ,  8,  12]
                   , [2     ,  11]
                   , [-7, -3,   9]
                   , [-7,  8,  -9]
                   , [7 ,  8, -10]
                   , [7 , 10, -12]])
     ]


