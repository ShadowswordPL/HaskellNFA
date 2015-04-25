import Auto
import System.Environment
import Text.Read
import Data.Char
import Data.List.Split
import Text.Regex.Posix

data CorrectInput = CorrectStateNum Int 
                  | CorrectStateList [Int]
                  | CorrectWord String
                  | CorrectTransition Int String [Int]
                  deriving (Show, Eq)

linesWithoutTransitionNum = 4

maybeStateNumToMaybeCorrectInput :: (Maybe Int) -> (Maybe CorrectInput)
maybeStateNumToMaybeCorrectInput Nothing = Nothing
maybeStateNumToMaybeCorrectInput (Just a) = Just (CorrectStateNum a)

maybeStateListToMaybeCorrectInput :: (Maybe [Int]) -> (Maybe CorrectInput)
maybeStateListToMaybeCorrectInput Nothing = Nothing
maybeStateListToMaybeCorrectInput (Just a) = Just (CorrectStateList a)

transitionPattern = "([0-9]+) ([A-Z]+)( ([0-9]+))+"

transitionStrings :: String -> (String, String, String, [String])
transitionStrings str = (str =~ transitionPattern :: (String, String, String, [String]))

transitionIsCorrect :: String -> Bool
transitionIsCorrect str = let
    emptyRegexPrefix = let
        (prefix, _, _, _) = (transitionStrings str)
        in null prefix
    emptyRegexSufix = let
        (_, _, sufix, _) = (transitionStrings str)
        in null sufix
    in (str =~ transitionPattern :: Bool) && emptyRegexPrefix && emptyRegexSufix

transitionToMaybeCorrectInput :: String -> (Maybe CorrectInput)
transitionToMaybeCorrectInput str = let
    (_, matchedString, _, subexpressionMatches) = transitionStrings str
    beginningState = read (head subexpressionMatches)
    word = subexpressionMatches!!1
    -- Subexpression matches list isn't complete i.e. 
    -- "1 AA 2 3 4" =~ "([0-9]+) ([A-Z]+)( ([0-9]+))+" :: (String, String, String, [String])
    -- is ("","1 AA 2 3 4","",["1","AA"," 4","4"])
    -- not ("","1 AA 2 3 4","",["1","AA","2"," 3", " 4"]) which I supposed
    -- so I have to get ["2", "3", "4"] list another way
    endStatesOneList :: String -- e.g. "2 3 4"
    endStatesOneList = drop (length (concat [(head subexpressionMatches), " ", word, " "])) matchedString
    endStatesList :: [String] -- e.g. ["2", "3", "4"]
    endStatesList = splitOn " " endStatesOneList
    endStates :: [Int] -- e.g. [2, 3, 4]
    endStates = map read endStatesList
    correctTransitionFromStringList = CorrectTransition beginningState word endStates
    
    in if (transitionIsCorrect str) then
        Just correctTransitionFromStringList
    else
        Nothing

wordIsCorrect :: String -> Bool
wordIsCorrect [] = True
wordIsCorrect (x:xs) = (x >= 'A') && (x <= 'Z') && (wordIsCorrect xs)

wordToMaybeCorrectInput :: String -> (Maybe CorrectInput)
wordToMaybeCorrectInput str = let
    in if (wordIsCorrect str) then Just (CorrectWord str) else Nothing

loadAutomat :: [String] -> Maybe Bool
loadAutomat linesList = let
    myAssert :: Bool -> Maybe Bool
    myAssert False = Nothing
    myAssert True = Just True
    
    checkFileLength :: CorrectInput -> Bool
    checkFileLength (CorrectStateNum stateNum) = ((length linesList) == (stateNum + linesWithoutTransitionNum))
    checkFileLength x = False
    
    allStatesFromTransitions :: [CorrectInput] -> [Int]
    allStatesFromTransitions list = let
        allStatesFromOneTransition (CorrectTransition beginningState word endStates) = beginningState:endStates
        allStatesFromTransitionHelper :: [CorrectInput] -> [Int] -> [Int]
        allStatesFromTransitionHelper [] result = result
        allStatesFromTransitionHelper (x:list) result = allStatesFromTransitionHelper list ((allStatesFromOneTransition x) ++ result)
        in allStatesFromTransitionHelper list [] 
    
    allTransitions :: [CorrectInput] -> [(Int, Char, [Int])]
    allTransitions list = let
        allTransitionsFromOneTransition (CorrectTransition beginningState word endStates) = [(beginningState, char, endStates) | char <- word]
        allTransitionsHelper :: [CorrectInput] -> [(Int, Char, [Int])] -> [(Int, Char, [Int])]
        allTransitionsHelper [] result = result
        allTransitionsHelper (x:list) result = allTransitionsHelper list ((allTransitionsFromOneTransition x) ++ result)
        in allTransitionsHelper list []

    createAutomat :: CorrectInput -> CorrectInput -> [CorrectInput] -> Auto Char Int 
    createAutomat (CorrectStateList initStates) (CorrectStateList acceptingStates) transitions = 
        fromLists (allStatesFromTransitions transitions) initStates acceptingStates (allTransitions transitions)
    
    checkAutomat :: Auto Char Int -> CorrectInput -> Bool
    checkAutomat automat (CorrectWord word) = accepts automat word
    in do
        validate <- myAssert (length linesList > linesWithoutTransitionNum)
        --converting Maybe Int (or [Int]) to Maybe CorrectInput is optional, but I like having complete CorrectInput type
        numOfStates <- maybeStateNumToMaybeCorrectInput (readMaybe (head linesList))
        validate <- myAssert (checkFileLength numOfStates)
        initStatesList <- maybeStateListToMaybeCorrectInput (readMaybe (linesList!!1))
        acceptingStatesList <- maybeStateListToMaybeCorrectInput (readMaybe (linesList!!2))
        transitionList <- mapM transitionToMaybeCorrectInput (init (drop 3 linesList))
        word <- wordToMaybeCorrectInput (last linesList)
        let automat = createAutomat initStatesList acceptingStatesList transitionList
        return (checkAutomat automat word)

resultToString Nothing = "BAD INPUT"
resultToString (Just x) = show x

main = do
    args <- getArgs
    fileString <- (readFile (head args))
    let linesList = lines fileString
    print (resultToString (loadAutomat linesList))
    
