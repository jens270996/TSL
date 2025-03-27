
module ASTPrinting.Implementation.Counter where
import qualified Data.Map as Map
import TSL.AST
import Control.Monad
type Variables = Map.Map Variable Integer



newtype Counter a = Counter
    { runCounter ::
        Variables ->
        Integer ->
        (a,Variables,Integer)
    }


instance Functor Counter where
    fmap = liftM

instance Applicative Counter where
    pure a = Counter (\vars c -> (a,vars,c))
    (<*>) = ap

instance Monad Counter where
    m >>= f =
        let transferFunction vars c =
                let (a,vars',c') = runCounter m vars c
                    result = f a
                in runCounter result vars' c'
        in Counter transferFunction

getIdentifier :: Variable -> Counter Integer
getIdentifier var = Counter (\vars c-> case Map.lookup var vars of
                                                        Just var' -> (var',vars,c)
                                                        Nothing -> (c+1,Map.insert var (c+1) vars, c+1))

emptyVariables :: Variables
emptyVariables = Map.empty