import Data.Char
import Data.List
import Data.Word
import Data.Bits
import qualified Data.Map as M

type Position = (Int, Int)

type State = [Position]

type EditDistance = Int

type KLength = Int

type Bitmask = Int

type CharacteristicVector = (Bitmask, KLength)

names = [s | i <- [0..], let s = if i == 0 then "ERR" else toBase26 i] 
  where toBase26 = toBase26' ""
        toBase26' s 0 = s
        toBase26' s n = let c = chr ((n `mod` 26) + (ord 'A') - 1) 
                            c' = if c == '@' then 'Z' else c
                        in toBase26' ([c'] ++ s) (n `div` 26)

delta :: EditDistance -> Position -> CharacteristicVector -> State
delta n (i, e) (x, k) 
  | k >= 2 && e <= n - 1 && x == 0  = [(i, e + 1), (i + 1, e + 1)]
  | k >= 2 && e <= n - 1 && y == 1  = [(i + 1, e)]
  | k >= 2 && e <= n - 1 && y > 1   = [(i, e + 1), (i + 1, e + 1), (i + y, e + y - 1)]
  | k == 1 && e <= n - 1            = case x .&. 1 of 
                                      0         -> [(i, e + 1), (i + 1, e + 1)]
                                      1         -> [(i + 1, e)]                              
  | k == 0 && e <= n - 1            = [(i, e + 1)]
  | k == 1 && e == n                = case x .&. 1 of 
                                      0         -> []
                                      1         -> [(i + 1, n)]    
  | k == 0 && e == n                = []
  | otherwise                       = []
  where y = fstBit x
        fstBit 0 = 0
        fstBit b = if testBit b 0 == True then 1 else 1 + fstBit (shiftR b 1)

subsumes :: Position -> Position -> Bool
subsumes (i, e) (j, f) = e < f && abs (j - i) <= f - e

reduce :: State -> State
reduce state = foldl f state state
  where f acc pos = case filter (\p -> p `subsumes` pos) acc of
                      []        -> acc
                      otherwise -> delete pos acc 

reducedUnion :: State -> State -> State
reducedUnion st1 st2 = reduce $ union st1 st2

powerSet :: EditDistance -> [CharacteristicVector]
powerSet n = [(mask, k) | k <- [0..(2 * n + 1)], mask <- [0..(2 ^ k) - 1]]

baseState :: State -> (State, Int)
baseState st = let smi = smallestI in ((map ((\minI (i, e) -> (i - minI, e)) smi) st), smi)
  where smallestI = foldl (\acc (i, e) -> min i acc) (2 * editDistance + 1) st

delta1 :: State -> CharacteristicVector -> State
delta1 [] _ = emptyState
delta1 st x = foldl f emptyState st
  where f acc p = reducedUnion acc $ delta editDistance p (x' p x)
        x' (i, e) (b, k) = let k' = min (editDistance - e + 1) (k - i) in
                            ((shiftR b i) .&. ((bit k') - 1), k')

delta' st = foldl f [] (powerSet editDistance)
  where f acc x = union acc [delta1 st x]

delta'' st = foldl f M.empty (powerSet editDistance)
  where f acc x = M.insert x (baseState (delta1 st x)) acc

generate = generate' M.empty [emptyState, initialState]
  where 
      generate' visited [] = visited
      generate' visited (s:st) = let t = delta'' s
                                     visited' = M.insert s t visited
                                     rest = st `union` (map fst $ M.elems t) \\ (M.keys visited')                                  
                                  in generate' visited' rest

generateStateNames :: [State] -> M.Map State String
generateStateNames = f' M.empty names
  where 
    f' m _ []          = m
    f' m (n:nx) (s:st) = f' (M.insert s n m) nx st

main = let states = generate in putStrLn $ printStates (generateStateNames (M.keys states)) states
--main = let states = generate in putStrLn $ printStates' states

printStates :: M.Map State String ->  M.Map State (M.Map (Int, Int) (State, Int)) -> String
printStates names = M.foldlWithKey (f' names) ""
  where f' names acc fromState transitions = M.foldlWithKey (g' names fromState) acc transitions
        g' names fromState acc charVector (toState, inc) = acc ++ (show (name names fromState)) ++ " --> " ++ (show charVector) ++ " --> (" ++ (show (name names toState)) ++ ", " ++ (show inc) ++ ")\n"   
        name names state = case M.lookup state names of 
                            Just (s) -> s
                            Nothing -> ""

printStates' ::  M.Map State (M.Map (Int, Int) (State, Int)) -> String
printStates' = M.foldlWithKey f' ""
  where f' acc fromState transitions = M.foldlWithKey (g' fromState) acc transitions
        g' fromState acc charVector (toState, inc) = acc ++ (show (fromState)) ++ " --> " ++ (show charVector) ++ " --> (" ++ (show (toState)) ++ ", " ++ (show inc) ++ ")\n"  


editDistance :: EditDistance
editDistance = 2

emptyState :: State
emptyState = []

initialState :: State
initialState = [(0, 0)]

