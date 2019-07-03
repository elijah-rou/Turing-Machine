{-
    RSSELI007

    Program to simulate a turing machine that decides whether a given string is a palindrome.
    Gamma = {a, b, NULL}
    Sigma = {a, b}
    8 states, incl. 1 reject and 1 accept state
-}
import Data.Sequence
import Data.Foldable
import Debug.Trace

-- Define a type that represents the state of the TM 
data State = Q0 | Q1 | Q2 | Q3 | Q4 | Q5 | QAccept | QReject
    deriving (Eq, Show)

-- Define a type that represents the configuration of a TM (WriteHead position, Current State, Tape) 
-- Full septuple is not needed
data TuringMachine = TuringMachine {pos :: Int, state :: State, tape :: Seq Char}
-- Show implementation for a TM
instance Show TuringMachine where
    show (TuringMachine p s t) =
        "Tape: " ++ (show (toList t)) ++ ", Head Position: " ++ (show p) ++ ", Current State: " ++ (show s) 

-- Define a type that specifies an action for the update head of the TM
data Action = L | R 
-- Function that advances the write head of a TM given an Action
moveHead :: Action -> Int -> Int
moveHead L tapeIndex = tapeIndex - 1;
moveHead R tapeIndex = tapeIndex + 1

-- The delta function of the TM
deltaFunction :: TuringMachine -> State -> Char -> TuringMachine
-- QAccept/QReject Transitions
-- Trivial Accept/Reject
deltaFucntion tm QAccept _ = TuringMachine {
    pos=pos tm, state=QAccept, tape=tape tm}
deltaFunction tm QReject _ = TuringMachine {
    pos=pos tm, state=QReject, tape=tape tm}
-- Transitions if the first symbol is a, for (Q0, Q1, Q2)
-- Replace initial with a blank, move right until a blank is reached
-- Check last character, if b reject, if blank accept, else move to Q3 
deltaFunction tm Q0 'a' = TuringMachine {
    -- (Q0, a) -> (Q1, _, R)
    pos=moveHead R (pos tm), state=Q1, tape=update (pos tm) '_' (tape tm)} 
deltaFunction tm Q1 'a' = TuringMachine {
    -- (Q1, a) -> (Q1, a, R)
    pos=moveHead R (pos tm), state=Q1, tape=tape tm} 
deltaFunction tm Q1 'b' = TuringMachine {
    -- (Q1, b) -> (Q1, b, R)
    pos=moveHead R (pos tm), state=Q1, tape=tape tm} 
deltaFunction tm Q1 '_' = TuringMachine {
    -- (Q1, _) -> (Q2, _, L)
    pos=moveHead L (pos tm), state=Q2, tape=tape tm} 
deltaFunction tm Q2 'a' = TuringMachine {
    -- (Q2, a) -> (Q3, _, L)
    pos=moveHead L (pos tm), state=Q3, tape=update (pos tm) '_' (tape tm)} 
deltaFunction tm Q2 'b' = TuringMachine {
    -- (Q2, b) -> (QReject, b, N)
    pos=pos tm, state=QReject, tape=tape tm} 
deltaFunction tm Q2 '_' = TuringMachine {
    -- (Q2, _) -> (QAccept, _, N)
    pos=pos tm, state=QAccept, tape=tape tm} 
-- Transitions if the first Symbol is b, for (Q0, Q4, Q5)
-- Replace initial with a blank, move right until a blank is reached
-- Check last character, if a reject, if blank accept, else move to Q3 
deltaFunction tm Q0 'b' = TuringMachine {
    -- (Q0, b) -> (Q4, _, R)
    pos=moveHead R (pos tm), state=Q4, tape=update (pos tm) '_' (tape tm)} 
deltaFunction tm Q4 'a' = TuringMachine {
    -- (Q4, a) -> (Q4, a, R)
    pos=moveHead R (pos tm), state=Q4, tape=tape tm} 
deltaFunction tm Q4 'b' = TuringMachine {
    -- (Q4, b) -> (Q4, b, R)
    pos=moveHead R (pos tm), state=Q4, tape=tape tm} 
deltaFunction tm Q4 '_' = TuringMachine {
    -- (Q4, _) -> (Q5, _, L)
    pos=moveHead L (pos tm), state=Q5, tape=tape tm} 
deltaFunction tm Q5 'a' = TuringMachine {
    -- (Q5, a) -> (QReject, a, N)
    pos=pos tm, state=QReject, tape=tape tm} 
deltaFunction tm Q5 'b' = TuringMachine {
    -- (Q5, b) -> (Q3, _, L)
    pos=moveHead L (pos tm), state=Q3, tape=update (pos tm) '_' (tape tm)} 
deltaFunction tm Q5 '_' = TuringMachine {
    -- (Q5, _) -> (QAccept, _, N)
    pos=pos tm, state=QAccept, tape=tape tm} 
-- Transitions for Q3
-- Move left to the first occurence of a symbol {a,b}
deltaFunction tm Q3 'a' = TuringMachine {
    -- (Q3, a) -> (Q3, a, L)
    pos=moveHead L (pos tm), state=Q3, tape=tape tm} 
deltaFunction tm Q3 'b' = TuringMachine {
    -- (Q3, b) -> (Q3, b, L)
    pos=moveHead L (pos tm), state=Q3, tape=tape tm} 
deltaFunction tm Q3 '_' = TuringMachine {
    -- (Q3, _) -> (Q0, _, R)
    pos=moveHead R (pos tm), state=Q0, tape=tape tm} 
-- Transition if the first symbol is Blank
-- Trivial Accept
deltaFunction tm Q0 '_' = TuringMachine {
    -- (Q0, _) -> (QAccept, _, N)
    pos=pos tm, state=QAccept, tape=tape tm} 

-- Function that runs (and terminates) the TM
runTM :: TuringMachine -> String
runTM tm    | (state tm) == QAccept = trace (show tm) "OUTPUT: a"
            | (state tm) == QReject = trace (show tm) "OUTPUT: b"
            | otherwise       = trace (show tm) 
                runTM (deltaFunction tm (state tm) (index (tape tm) (pos tm)))
-- Creates a tape representation in the form of a character seq
createTape :: String -> Seq Char
createTape string = fromList ("_" ++ string ++ "_")
-- Function that calculates the palindrome of a string over {a,b} using a TM
-- TM initial config set to {1, Q0, tape}
palindromeTM :: String -> String
palindromeTM string = 
    runTM (TuringMachine {pos=1, state=Q0, tape=createTape string})

