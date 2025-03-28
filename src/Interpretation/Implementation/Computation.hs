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
type Trace = [String]

emptyVariableStore :: VariableStore
emptyVariableStore = Map.empty
constructInitialStores :: Program -> FunctionStore
constructInitialStores p =
    let involutionStore = Map.fromList ( zip  (involutionIds p) (involutions p))
        procedureStore = Map.fromList  (zip (procedureIds p) (procedures p))
    in (procedureStore,involutionStore)



newtype Computation a = Computation
    { runComputation ::
        Trace ->
        FunctionStore ->
        VariableStore ->
        ErrorMonad (a,VariableStore,Trace)
    }


instance Functor Computation where
    fmap = liftM

instance Applicative Computation where
    pure :: a -> Computation a
    pure a = Computation (\trace _ vars -> return (a, vars,trace))
    (<*>) = ap

instance Monad Computation where
    m >>= f =
        let transferFunction trace funcs vars =
                do (a, vars',trace') <- runComputation m trace funcs vars
                   let result = f a
                   runComputation result trace' funcs vars'
        in Computation transferFunction


getInvolutionC :: Identifier -> Computation Involution
getInvolutionC id = Computation (\trace (_,invols) vars -> case Map.lookup id invols of
                                                        Just invol -> Right (invol, vars,trace)
                                                        Nothing -> Left $ "Trying to lookup undeclared involution: " ++ id
                                )
getProcedureC :: Identifier -> Computation Procedure
getProcedureC id = Computation (\trace (procs,_) vars -> case Map.lookup id procs of
                                                        Just procedure -> Right (procedure, vars,trace)
                                                        Nothing -> Left $ "Trying to lookup undeclared procedure: " ++ id
                                )
lookC :: Variable -> Computation Constant
lookC var = Computation (\trace (_,_) vars -> case Map.lookup var vars of
                                                        Just c -> Right (c, vars,trace)
                                                        Nothing -> Right (Nil, vars,trace)
                         )
setC :: Variable -> Constant -> Computation ()
setC var c = Computation (\trace (_,_) vars -> case c of
                                            Nil -> Right ((), Map.delete var vars,trace)
                                            _ -> case Map.lookup var vars of
                                                    Just _ -> Left $ "Trying to perform destructive assignment of variable: " ++ var
                                                    _ ->      Right ((),Map.insert var c vars,trace))

getC :: Variable -> Computation Constant
getC var = do c <- lookC var
              resetC var
              return c

resetC :: Variable -> Computation ()
resetC var = Computation (\trace (_,_) vars -> Right ((),Map.delete var vars,trace))

overrideC :: Variable -> Constant -> Computation ()
overrideC var c = Computation (\trace (_,_) vars -> case c of
                                            Nil -> Right ((), Map.delete var vars,trace)
                                            _ -> Right ((),Map.insert var c vars,trace))

-- TODO: extend with env
throwC :: String -> Computation ()
throwC e = Computation (\trace (_,_) varStore -> Left $ "Environment contained: \n"++ printStore varStore ++ "Trace contained: \n"++ printTrace trace ++ "\nCaught error: " ++ e  ++ "\n")

printTrace :: Trace -> String
printTrace t = "[" ++ concat ((map (++ ",\n")) . reverse $ t) ++ "]\n"

printStore :: VariableStore -> String
printStore = show


assertEnvironmentEmptyC :: Computation ()
assertEnvironmentEmptyC = Computation (\trace _ vars -> if vars == Map.empty then Right ((),vars,trace) else Left $ "Environment must be empty at return from function. Non-empty vars: " ++ (show vars))

getEnvironmentC :: Computation VariableStore
getEnvironmentC = Computation (\trace _ vars -> Right (vars,vars,trace))

withEnvironmentC :: VariableStore -> Computation ()
withEnvironmentC vars = Computation (\trace _ _ -> Right ((),vars,trace))

traceC :: String -> Computation ()
traceC s = Computation (\trace _ vars -> Right ((),vars,(s:trace)))
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
  assertEnvironmentEmpty :: cm ()
  trace :: String -> cm ()
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
    assertEnvironmentEmpty = assertEnvironmentEmptyC
    override = overrideC
    trace = traceC
