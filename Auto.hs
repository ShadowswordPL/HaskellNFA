module Auto ( Auto
            , accepts
            , emptyA
            , epsA
            , symA
            , leftA
            , sumA
            , thenA
            , fromLists
            , toLists
            ) where 

import Data.List
import Debug.Trace

data Auto a q = A { states :: [q]
                  , initStates :: [q]
                  , isAccepting :: q -> Bool
                  , transition :: q -> a -> [q]
                  }

instance (Show a, Show q, Enum a, Bounded a) => Show (Auto a q) where
    show automat = show (toLists automat)


accepts :: (Eq q, Show q) => Auto a q -> [a] -> Bool
accepts automat word = let
    possibleStatesFromPair (beginningState, letter) = (transition automat) beginningState letter

    acceptsHelper beginningStates [] = any (isAccepting automat) beginningStates
    acceptsHelper [] word = False
    acceptsHelper beginningStates word = acceptsHelper (nub (concatMap possibleStatesFromPair [(beginningState, head word) | beginningState <- beginningStates])) (tail word)
    in acceptsHelper (initStates automat) word


emptyA :: Auto a ()
emptyA = let
    in A { states = []
         , initStates = []
         , isAccepting = (\q -> False)
         , transition = (\q -> \a -> [])
         }


epsA :: Auto a ()
epsA = let
    in A { states = [()]
         , initStates = [()]
         , isAccepting = (\q -> True)
         , transition = (\q -> \a -> [])
         }


symA :: Eq a => a -> Auto a Bool
symA c = let
    isAcceptingFunc q = q
    transitionFunc False x = if x == c then [True] else []
    transitionFunc True x = []

    in A { states = [False, True]
         , initStates = [False]
         , isAccepting = isAcceptingFunc
         , transition = transitionFunc
         }


leftA :: Auto a q -> Auto a (Either q r)
leftA automat = let
    isAcceptingFunc (Left q) = (isAccepting automat) q
    transitionFunc (Left q) a = map Left ((transition automat) q a)

    in A { states = map Left (states automat)
         , initStates = map Left (initStates automat)
         , isAccepting = isAcceptingFunc
         , transition = transitionFunc
         } 

toEither :: [q1] -> [q2] -> [Either q1 q2]
toEither list1 list2 = concat [[Left x | x <- list1], [Right y | y <- list2]]

sumA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
sumA automat1 automat2 = let
    transitionFunc (Left q) a = map Left ((transition automat1) q a)
    transitionFunc (Right q) a = map Right ((transition automat2) q a)

    in A { states = toEither (states automat1) (states automat2)
         , initStates = toEither (initStates automat1) (initStates automat2)
         , isAccepting = either (isAccepting automat1) (isAccepting automat2)
         , transition = transitionFunc
         } 


thenA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
thenA automat1 automat2 = let
    ifFirstAutoAcceptsEmpty = if (any (isAccepting automat1) (initStates automat1)) then map Right (initStates automat2) else [] 
    transitionsFromPair (s, l) = (transition automat2) s l
    mergingTransitions q a = if (isAccepting automat1) q then
        concat (map transitionsFromPair [(x, a) | x <- (initStates automat2)])
    else
        []
    transitionFunc (Left q) a = concat [map Left ((transition automat1) q a), map Right (mergingTransitions q a)]
    transitionFunc (Right q) a = map Right ((transition automat2) q a)

    in A { states = toEither (states automat1) (states automat2)
         , initStates = map Left (initStates automat1) ++ ifFirstAutoAcceptsEmpty
         , isAccepting = either (\x -> False) (isAccepting automat2)
         , transition = transitionFunc
         }

fromLists :: (Eq q, Eq a) => [q] -> [q] -> [q] -> [(q,a,[q])] -> Auto a q
fromLists statesArg initStatesArg acceptingArg transitionArg = let
    getThird (first, second, third) = third
    isAcceptingFunc q = elem q acceptingArg
    transitionFunc q a = concat (map getThird (filter (\(state, letter, _) -> (state, letter) == (q, a)) transitionArg))

    in A { states = statesArg
         , initStates = initStatesArg
         , isAccepting = isAcceptingFunc
         , transition = transitionFunc
         }


toLists :: (Enum a, Bounded a) => Auto a q -> ([q],[q],[q],[(q,a,[q])])
toLists automat = let
    transitionsFromPair (s, l) = (s, l, (transition automat) s l)

    in ( states automat,
         initStates automat,
         filter (isAccepting automat) (states automat),
         map transitionsFromPair [(s, l) | s <- (states automat), l <- [minBound..maxBound], not (null ((transition automat) s l))]
    )
