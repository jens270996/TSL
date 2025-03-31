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
        Trace ->
        FunctionStore ->
        VariableStore ->
        ErrorMonad (a,VariableStore,Trace,CallStack)
    }


instance Functor Computation where
    fmap = liftM

instance Applicative Computation where
    pure :: a -> Computation a
    pure a = Computation (\cs trace _ vars -> return (a, vars,trace,cs))
    (<*>) = ap

instance Monad Computation where
    m >>= f =
        let transferFunction cs trace funcs vars =
                do (a, vars',trace',cs') <- runComputation m cs trace funcs vars
                   let result = f a
                   runComputation result cs' trace' funcs vars'
        in Computation transferFunction


getInvolutionC :: Identifier -> Computation Involution
getInvolutionC id = Computation (\cs trace (_,invols) vars -> case Map.lookup id invols of
                                                        Just invol -> Right (invol, vars,trace,cs)
                                                        Nothing -> Left $ "Trying to lookup undeclared involution: " ++ id
                                )
getProcedureC :: Identifier -> Computation Procedure
getProcedureC id = Computation (\cs trace (procs,_) vars -> case Map.lookup id procs of
                                                        Just procedure -> Right (procedure, vars,trace,cs)
                                                        Nothing -> Left $ "Trying to lookup undeclared procedure: " ++ id
                                )
lookC :: Variable -> Computation Constant
lookC var = Computation (\cs trace (_,_) vars -> case Map.lookup var vars of
                                                        Just c -> Right (c, vars,trace,cs)
                                                        Nothing -> Right (Nil, vars,trace,cs)
                         )
setC :: Variable -> Constant -> Computation ()
setC var c = Computation (\cs trace (_,_) vars -> case c of
                                            Nil -> Right ((), Map.delete var vars,trace,cs)
                                            _ -> case Map.lookup var vars of
                                                    Just _ -> Left $ "Trying to perform destructive assignment of variable: " ++ var
                                                    _ ->      Right ((),Map.insert var c vars,trace,cs))

getC :: Variable -> Computation Constant
getC var = do c <- lookC var
              resetC var
              return c

resetC :: Variable -> Computation ()
resetC var = Computation (\cs trace (_,_) vars -> Right ((),Map.delete var vars,trace,cs))

overrideC :: Variable -> Constant -> Computation ()
overrideC var c = Computation (\cs trace (_,_) vars -> case c of
                                            Nil -> Right ((), Map.delete var vars,trace,cs)
                                            _ -> Right ((),Map.insert var c vars,trace,cs))

printError :: CallStack -> Trace -> VariableStore -> String -> String
printError cs t v s =
    "Environment contained: \n"++ printStore v ++ "\nTrace contained: \n"++ printTrace t ++ "\nCallstack contained: \n"++ printCallStack cs ++ "\nCaught error: " ++ s  ++ "\n"

throwC :: String -> Computation ()
throwC e = Computation (\cs trace (_,_) varStore -> Left $ printError cs trace varStore e )

printCallStack :: CallStack -> String
printCallStack cs = "[" ++ (concatMap (++ ",\n\n") . take 10 . reverse $ cs) ++ "]\n"
printTrace :: Trace -> String
printTrace t = "[" ++ concat ((map (++ ",\n\n")) . reverse $ t) ++ "]\n"

printStore :: VariableStore -> String
printStore = show


assertEnvironmentEmptyC :: Computation ()
assertEnvironmentEmptyC = Computation (\cs trace _ vars -> if vars == Map.empty
                                                        then Right ((),vars,trace,cs)
                                                        else Left $ printError cs trace vars
                                                                "Environment must be empty at return from function.")

getEnvironmentC :: Computation VariableStore
getEnvironmentC = Computation (\cs trace _ vars -> Right (vars,vars,trace,cs))

withEnvironmentC :: VariableStore -> Computation ()
withEnvironmentC vars = Computation (\cs trace _ _ -> Right ((),vars,trace,cs))

traceC :: String -> Computation ()
traceC s = Computation (\cs trace _ vars -> Right ((),vars,(s:trace),cs))

callC :: String -> Computation ()
callC s = Computation (\cs trace _ vars -> Right ((),vars,trace,s:cs))
returnC :: Computation ()
returnC = Computation (\cs trace _ vars -> Right ((),vars,trace,tail cs))

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
  trace :: String -> cm ()
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
    trace = traceC
    call = callC
    exit =assertEnvironmentEmptyC >> returnC