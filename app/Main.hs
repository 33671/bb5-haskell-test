data State = A | B | C | D | E | Halt deriving (Eq, Show)

data Symbol = Zero | One deriving (Eq, Show)

data Direction = L | R deriving (Eq, Show)

type Tape = ([Symbol], Symbol, [Symbol])

type Steps = Int

type WriteCount = Int

-- tape head
move :: Tape -> Direction -> Tape
move (ls, s, []) R = (s : ls, Zero, [])
move (ls, s, r : rs) R = (s : ls, r, rs)
move ([], s, rs) L = ([], Zero, s : rs)
move (l : ls, s, rs) L = (ls, l, s : rs)

-- rules
step :: State -> Symbol -> (Symbol, Direction, State)
step A Zero = (One, R, B)
step A One = (One, L, C)
step B Zero = (One, R, C)
step B One = (One, R, B)
step C Zero = (One, R, D)
step C One = (Zero, L, E)
step D Zero = (One, L, A)
step D One = (One, L, D)
step E Zero = (One, R, Halt)
step E One = (Zero, L, A)
step _ _ = error "Invalid state or symbol"

executeStep :: (State, Tape, Steps, WriteCount) -> (State, Tape, Steps, WriteCount)
executeStep (state, (ls, s, rs), steps, writeCount) =
  let (writeSymbol, moveDir, nextState) = step state s
      newTape = move (ls, writeSymbol, rs) moveDir
      newWriteCount = if writeSymbol == One then writeCount + 1 else writeCount
   in (nextState, newTape, steps + 1, newWriteCount)

initTape :: Tape
initTape = ([], Zero, repeat Zero)

runMachine :: (State, Tape, Steps, WriteCount) -> Steps
runMachine (Halt, _, steps, _) =
  steps
runMachine stateTapeStepsWriteCount =
  runMachine $ executeStep stateTapeStepsWriteCount

main :: IO ()
main = do
  let initialMachine = (A, initTape, 0, 0)
  let count = runMachine initialMachine in print count