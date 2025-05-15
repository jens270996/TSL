{-# LANGUAGE InstanceSigs #-}
module Interpretation.Implementation.Computation where

import qualified Data.Map as Map
import TSL.AST
import Utils.Error
import Control.Monad
import Utils.AST

type VariableStore = Map.Map Variable Constant
type ProcedureStore = Map.Map Identifier Procedure
type InvolutionStore = Map.Map Identifier Involution
type FunctionStore = (ProcedureStore,InvolutionStore)
type CallStack = [String]

emptyVariableStore :: VariableStore
emptyVariableStore = Map.empty
constructInitialStores :: Program -> FunctionStore
constructInitialStores p =
    let involutionStore = Map.fromList ( zip  (involutionIds p) (involutions p))
        procedureStore = Map.fromList  (zip (procedureIds p) (procedures p))
    in (procedureStore,involutionStore)



newtype Computation a = Computation
    { runComputation ::
        CallStack ->
        FunctionStore ->
        VariableStore ->
        ErrorMonad (a,VariableStore,CallStack)
    }


instance Functor Computation where
    fmap = liftM

instance Applicative Computation where
    pure :: a -> Computation a
    pure a = Computation (\cs _ vars -> return (a, vars,cs))
    (<*>) = ap

instance Monad Computation where
    m >>= f =
        let transferFunction cs funcs vars =
                do (a, vars',cs') <- runComputation m cs funcs vars
                   let result = f a
                   runComputation result cs' funcs vars'
        in Computation transferFunction


getInvolutionC :: Identifier -> Computation Involution
getInvolutionC id = Computation (\cs (_,invols) vars -> case Map.lookup id invols of
                                                        Just invol -> Right (invol, vars,cs)
                                                        Nothing -> Left $ "Trying to lookup undeclared involution: " ++ id
                                )
getProcedureC :: Identifier -> Computation Procedure
getProcedureC id = Computation (\cs (procs,_) vars -> case Map.lookup id procs of
                                                        Just procedure -> Right (procedure, vars,cs)
                                                        Nothing -> Left $ "Trying to lookup undeclared procedure: " ++ id
                                )
lookC :: Variable -> Computation Constant
lookC var = Computation (\cs (_,_) vars -> case Map.lookup var vars of
                                                        Just c -> Right (c, vars,cs)
                                                        Nothing -> Right (Nil, vars,cs)
                         )
setC :: Variable -> Constant -> Computation ()
setC var c = Computation (\cs (_,_) vars -> case c of
                                            Nil -> Right ((), Map.delete var vars,cs)
                                            _ -> case Map.lookup var vars of
                                                    Just _ -> Left $ "Trying to perform destructive assignment of variable: " ++ var
                                                    _ ->      Right ((),Map.insert var c vars,cs))

getC :: Variable -> Computation Constant
getC var = do c <- lookC var
              resetC var
              return c

resetC :: Variable -> Computation ()
resetC var = Computation (\cs (_,_) vars -> Right ((),Map.delete var vars,cs))

overrideC :: Variable -> Constant -> Computation ()
overrideC var c = Computation (\cs (_,_) vars -> case c of
                                            Nil -> Right ((), Map.delete var vars,cs)
                                            _ -> Right ((),Map.insert var c vars,cs))

printError :: CallStack -> VariableStore -> String -> String
printError cs v s =
    "Environment contained: \n"++ printStore v ++ "\nCallstack contained: \n"++ printCallStack cs ++ "\nCaught error: " ++ s  ++ "\n"

throwC :: String -> Computation ()
throwC e = Computation (\cs (_,_) varStore -> Left $ printError cs varStore e )

printCallStack :: CallStack -> String
printCallStack cs = "[" ++ (concatMap (++ ",\n\n") . reverse $ cs) ++ "]\n"

printStore :: VariableStore -> String
printStore = show

assertEnvironmentEmptyC :: Computation ()
assertEnvironmentEmptyC = Computation (\cs _ vars -> if vars == Map.empty
                                                        then Right ((),vars,cs)
                                                        else Left $ printError cs vars
                                                                "Environment must be empty at return from function.")

getEnvironmentC :: Computation VariableStore
getEnvironmentC = Computation (\cs _ vars -> Right (vars,vars,cs))

withEnvironmentC :: VariableStore -> Computation ()
withEnvironmentC vars = Computation (\cs _ _ -> Right ((),vars,cs))


callC :: String -> Computation ()
callC s = Computation (\cs _ vars -> Right ((),vars,s:cs))
returnC :: Computation ()
returnC = Computation (\cs _ vars -> Right ((),vars,tail cs))

-- The class of monads that support the core RWS operations
class Monad cm => ComputationMonad cm where
  -- set variable to a value. Variable must be Nil prior to update.
  set :: Variable -> Constant -> cm ()
  -- reset variable to Nil.
  reset :: Variable -> cm ()
  -- get value of variable and set it to Nil.
  get :: Variable -> cm Constant
  -- get value of variable.
  look :: Variable -> cm Constant
  -- set variable to a value. Variable can have any value prior to update.
  -- Can cause destructive assignment.
  override :: Variable -> Constant -> cm ()
  -- Get procedure by its identifier
  getProcedure :: Identifier -> cm Procedure
  -- Get involution by its identifier
  getInvolution :: Identifier -> cm Involution
  getEnvironment :: cm VariableStore
  withEnvironment :: VariableStore -> cm ()
  call :: String -> cm ()
  exit :: cm()
  -- Throw an error
  throw :: String -> cm ()

instance ComputationMonad Computation where
    set = setC
    reset = resetC
    get = getC
    look = lookC
    getProcedure = getProcedureC
    getInvolution = getInvolutionC
    throw = throwC
    withEnvironment = withEnvironmentC
    getEnvironment = getEnvironmentC
    override = overrideC
    call = callC
    exit =assertEnvironmentEmptyC >> returnC