module Interpretation.Implementation.Computation where

import qualified Data.Map as Map
import TSL.AST
import Utils.Error
import Control.Monad
import Utils.AST

type VariableStore = Map.Map Variable Constant
type ProcedureStore = Map.Map Identifier Procedure
type InvolutionStore = Map.Map Identifier Involution

constructInitialStores :: Program -> (ProcedureStore,InvolutionStore)
constructInitialStores p =
    let involutionStore = Map.fromList ( zip  (involutionIds p) (involutions p))
        procedureStore = Map.fromList  (zip (procedureIds p) (procedures p))
    in (procedureStore,involutionStore)



newtype Computation a = Computation
    { runComputation ::
        (ProcedureStore,InvolutionStore) ->
        VariableStore ->
        ErrorMonad (a,VariableStore)
    }


instance Functor Computation where
    fmap = liftM

instance Applicative Computation where
    pure a = Computation (\ _ vars -> return (a, vars))
    (<*>) = ap

instance Monad Computation where
    m >>= f =
        let transferFunction funcs vars =
                do (a, vars') <- runComputation m funcs vars
                   let result = f a
                   runComputation result funcs vars'
        in Computation transferFunction


getInvolutionC :: Identifier -> Computation Involution
getInvolutionC id = Computation (\(_,invols) vars -> case Map.lookup id invols of
                                                        Just invol -> Right (invol, vars)
                                                        Nothing -> Left $ "Trying to lookup undeclared involution: " ++ id
                                )
getProcedureC :: Identifier -> Computation Procedure
getProcedureC id = Computation (\(procs,_) vars -> case Map.lookup id procs of
                                                        Just procedure -> Right (procedure, vars)
                                                        Nothing -> Left $ "Trying to lookup undeclared procedure: " ++ id
                                )
lookC :: Variable -> Computation Constant
lookC var = Computation (\(_,_) vars -> case Map.lookup var vars of
                                                        Just c -> Right (c, vars)
                                                        Nothing -> Right (Nil, vars)
                         )
setC :: Variable -> Constant -> Computation ()
setC var c = Computation (\(_,_) vars -> case c of
                                            Nil -> Right ((), Map.delete var vars)
                                            _ -> case Map.lookup var vars of
                                                    Just _ -> Left $ "Trying to perform destructive assignment of variable: " ++ var
                                                    _ ->      Right ((),Map.insert var c vars))

getC :: Variable -> Computation Constant
getC var = do c <- lookC var
              resetC var
              return c

resetC :: Variable -> Computation ()
resetC var = Computation (\(_,_) vars -> Right ((),Map.delete var vars))

overrideC :: Variable -> Constant -> Computation ()
overrideC var c = Computation (\(_,_) vars -> case c of
                                            Nil -> Right ((), Map.delete var vars)
                                            _ -> Right ((),Map.insert var c vars))

throwC :: String -> Computation ()
throwC e = Computation (\(_,_) _ -> Left e)


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
    override = overrideC
