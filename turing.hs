-- Turing machine interpreter written in haskell
import System.IO
import System.Environment
import Data.List

type Symbol = Char
type State = String
data Direction = L | R | N deriving (Read, Show)
blankSym = '⎵'

-- Formal definition of a Turing Machine as a 7-tuple
-- (Q, Γ, ⎵, Σ, ð, q0, F)
data Automaton = Automaton {
	states :: [State],
	tapeAlpha :: [Symbol],
	blankSymbol :: Symbol,
	inputAlpha :: [Symbol],
	delta :: (State -> Symbol -> (Symbol, Direction, State)),
	startState :: State,
	acceptStates :: [State]
}

-- Data type for the state of an operating turing machine
data OpState = OpState {
	tape :: [Symbol],
	headPosition :: Int,
	machineState :: State
} deriving (Read)

instance Show OpState where
	show (OpState tape headPosition machineState) =
		'╭'
		: (take headPosition spaces)
		++ machineState
		++ (take ((length tape) - (length machineState) - (headPosition)) spaces)
		++ "╮\n╰"
		++ tape
		-- We may have to fill up because of long state names
		++ (take (headPosition + (length machineState) - (length tape)) spaces)
		++ "╯"
		where spaces = repeat ' '

-- We need to do this to satisfy the Show typeclass
instance Show Automaton where
	show (Automaton q tAlpha blank iAlpha _ q0 acceptStates) =
		"("
		++ show q
		++ ", "
		++ show tAlpha
		++ ", "
		++ (blank
		: ", "
		++ iAlpha
		-- We really don't want to dump functions to stdout
		++ ", ð, "
		++ show acceptStates
		++ ")")


first :: (a, b, c) -> a
first (a, _, _) = a

second :: (a, b, c) -> b
second (_, b, _) = b

third :: (a, b, c) -> c
third (_, _, c) = c

-- This is for convenience
emptyMachine :: Automaton
emptyMachine = Automaton ["A"] ['a'] blankSym ['a'] (\x y -> (blankSym, N, "A")) "A" ["A"]

-- the logical state of a turing machine is given by its 'state' and the tape's state
step :: Automaton -> OpState -> Maybe OpState
step machine state =
	if (machineState state) `elem` (acceptStates machine)
		-- We're in a halting state, there's nowhere to go
		then Nothing
		else Just (handleState $ handleMovement $ handleOutput state)
			where
				handleOutput s = writeSymbol (first next) s
				handleMovement s = case second next of
					L -> moveLeft s
					N -> s
					R -> moveRight s
				handleState s = OpState (tape s) (headPosition s) (third next)
				next = (delta machine) (machineState state) ((tape state)!!(headPosition state))
	
	

moveLeft :: OpState -> OpState
moveLeft (OpState tape headPosition mState) =
	if headPosition == 0
		then OpState (blankSym:tape) 0 mState
		else OpState tape (headPosition - 1) mState


moveRight :: OpState -> OpState
moveRight (OpState tape headPosition mState) =
	OpState paddedTape (headPosition + 1) mState
		where paddedTape = if headPosition == length tape - 1
			then (tape ++ [blankSym])
			else tape


writeSymbol :: Symbol -> OpState -> OpState
writeSymbol sym (OpState tape headPosition mState) =
	OpState newTape headPosition mState
		where newTape = (take headPosition tape)
			++ [sym]
			++ (drop (headPosition + 1) tape)

main :: IO ()
main = do
	args <- getArgs
	-- if no parameter is given, we look for "conf" in the current directory
	let conf = if (length args) /= 0
		then args!!0
		else "./conf"
	let input = if (length args) < 2
		then [blankSym]
		else args!!1
	machine <- loadConf conf
	let startConfig = OpState input 0 (startState machine)
	putStrLn $ show startConfig
	simulateMachine machine startConfig

-- "Inner run loop", if you will
simulateMachine :: Automaton -> OpState -> IO ()
simulateMachine machine state = do
	case step machine state of
		Nothing       -> return ()
		Just newState -> do
			putStrLn $ show newState
			simulateMachine machine newState

-- Reads and parses config and returns an Automaton if the config was well-formed
loadConf :: FilePath -> IO Automaton
loadConf path = do
	rawConf <- readFile path
	let filteredConf = commentLess $ lines rawConf

	let states = read (filteredConf!!0) :: [State]
	let tAlphabet = read (filteredConf!!1) :: [Symbol]
	let iAlphabet = blankSym : tAlphabet
	let acceptStates = read (filteredConf!!2) :: [State]
	let delta = parseDelta $ map words $ drop 3 filteredConf
	let tm = Automaton states tAlphabet blankSym iAlphabet delta (states!!0) acceptStates
	return tm
		where
			commentLess [] = []
			commentLess (x:xs)
				| x == "" || (head x == '#') = commentLess xs
				| otherwise 				 = x:(commentLess xs)


-- Parses the function table of the transition function
-- The expected input is a list of rows, separated into words
parseDelta :: [[String]] -> (State -> Symbol -> (Symbol, Direction, State))
parseDelta funcTable state sym =
	let
		inputInd = case ind of
			Just val -> val + 1
			Nothing -> error "Transistion function definition not exhaustive: input symbol not found"
		-- Since we don't know which states are halting states, we can't just jump into one here.
		stateRow [] = error "transition Function definition not exhaustive: state not found"
		stateRow (x:xs) = if (head x) == state
			then x
			else stateRow xs
	in read ((stateRow funcTable)!!(inputInd)) :: (Symbol,Direction,State)
			where ind = elemIndex sym (map head (funcTable!!0))


