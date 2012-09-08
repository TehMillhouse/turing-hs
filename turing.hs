-- Turing machine interpreter written in haskell
import System.IO


type Symbol = Char
type State = String
data Direction = L | R | N

-- Formal definition of a Turing Machine as a 7-tuple
-- (Q, Γ, _, Σ, ð, q0, F)
data Automaton = Automaton { 
	q :: [State],
	tAlpha :: [Symbol],
	blank :: Symbol,
	iAlpha :: [Symbol],
	delta :: (State -> Symbol -> (State, Direction)),
	q0 :: State,
	acceptStates :: [State]
}


-- We need to do this to satisfy the Show typeclass
instance Show Automaton where
	show (Automaton q tAlpha blank iAlpha _ q0 acceptStates) = 
		"(" 
		++ show q 
		++ ", " 
		++ show tAlpha 
		++ ", " 
		++ show blank 
		++ ", " 
		++ show iAlpha 
		++ ", ð, " 
		++ show acceptStates 
		++ ")"


-- This is for convenience
emptyMachine :: Automaton
emptyMachine = Automaton ["A"] ['a'] ' ' ['a'] (\x y -> ("A", N)) "A" ["A"] 
