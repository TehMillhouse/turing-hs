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


main :: IO ()
main = do
	m <- loadConf "./conf"
	putStrLn $ show $ m


loadConf :: FilePath -> IO Automaton
loadConf path = do
	rawConf <- readFile path
	let filteredConf = commentLess $ lines rawConf

	let states = read (filteredConf!!0) :: [State]
	let tAlphabet = read (filteredConf!!1) :: [Symbol]
	let blank = read (filteredConf!!2) :: Symbol
	let iAlphabet = blank : tAlphabet
	let acceptStates = read (filteredConf!!3) :: [State]
	let delta = parseDelta $ drop 4 filteredConf
	return $ Automaton states tAlphabet blank iAlphabet delta (states!!0) acceptStates
		where
			commentLess [] = []
			commentLess (x:xs)
				| x == "" || (head x == '#') = commentLess xs
				| otherwise 				 = x:(commentLess xs)


parseDelta :: [String] -> (State -> Symbol -> (State, Direction))
parseDelta funTable = (\x y -> ("A",N))




