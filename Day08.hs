module Day08 where

import Control.Applicative
import Control.Monad.State
import Text.Parsec hiding ((<|>), State)


data Instruction = Acc Int | Jmp Int | Nop Int deriving Show
data ExecutionState = ExecutionState { ips :: [Int], acc :: Int } deriving Show

supportedInstructions :: [(String, Int -> Instruction)]
supportedInstructions =
  [ ("acc", Acc)
  , ("jmp", Jmp)
  , ("nop", Nop)
  ]

main :: IO ()
main = do
  (solution1, solution2) <- solve <$> readFile "Day08_input.txt"
  putStr "Part one: " >> print solution1
  putStr "Part two: " >> print solution2

solve :: String -> (Int, Int)
solve input = (accAfter executeUntilCycle, accAfter executeCorrect)
  where Right instructions = parse code "(error)" input
        accAfter executor = evalState (executor instructions >> gets acc) initialState
        initialState = ExecutionState { ips = [0], acc = 0 }

type StateExec = State ExecutionState

executeUntilCycle :: [Instruction] -> StateExec Bool
executeUntilCycle instructions = do
  s <- get
  let ips' = ips s
  let ip' = head ips'
  if ip' == length instructions then return True
  else if ip' `notElem` tail ips' then execute (instructions !! ip') >> executeUntilCycle instructions
  else return False

executeCorrect :: [Instruction] -> StateExec Bool
executeCorrect instructions = do
  initialState <- get
  success <- executeUntilCycle instructions
  if not success then do
    ips' <- gets ips
    let ip' = head ips'
    let potentiallyFaultyIndices = ip' : takeWhile (/= ip') (tail ips')
    let folder idx s' = do
        skip <- s'
        if not skip then
          case tryFix idx of
            Just fixed -> put initialState >> executeUntilCycle fixed
            Nothing    -> return False
        else return True
    foldr folder (return False) potentiallyFaultyIndices
  else return success
  where tryFix idx = let fixed = tryFixInstruction (instructions !! idx)
                     in (\v -> swapElementAt v idx instructions) <$> fixed

tryFixInstruction :: Instruction -> Maybe Instruction
tryFixInstruction (Jmp n) = Just (Nop n)
tryFixInstruction (Nop n) = Just (Jmp n)
tryFixInstruction _       = Nothing

swapElementAt :: a -> Int -> [a] -> [a]
swapElementAt value idx list = left ++ [value] ++ right
  where (left, _ : right) = splitAt idx list

execute :: Instruction -> StateExec ()
execute i = state $ (,) () . execute' i
  where execute' (Acc n) = incIp 1 . incAcc n
        execute' (Jmp n) = incIp n
        execute' (Nop _) = incIp 1
        incAcc n s = s { acc = acc s + n }
        incIp n s = let ips' = ips s in s { ips = head ips' + n : ips' }

type Parser = Parsec String ()

code :: Parser [Instruction]
code = manyTill (tryInstructions <* endOfLine) end
  where tryInstructions = foldr folder empty supportedInstructions
        folder (name, ctor) parser = try (instruction name ctor) <|> parser
        end = eof <|> () <$ endOfLine

instruction :: String -> (Int -> Instruction) -> Parser Instruction
instruction name constructor = constructor <$> (string name *> space *> argument)

argument :: Parser Int
argument = sign <*> number
  where sign =  id <$ char '+'
            <|> negate <$ char '-'
        number = read <$> many1 digit
