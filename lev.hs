import Data.Char
import Data.List
import Data.Word
import Data.Bits
import Data.Maybe
import qualified Data.Map as M

type Position = (Int, Int)

type State = [Position]

type EditDistance = Int

type KLength = Int

type Bitmask = Int

type CharacteristicVector = (Bitmask, KLength)

type StateNameMap = M.Map State String

type StateDelta = (State, Int)

type TransitionMap = M.Map CharacteristicVector StateDelta

type StateTransitionMap = M.Map State TransitionMap

names = [s | i <- [0..], let s = if i == 0 then "ERR" else (reverse . map int2let . base26) i] 
    where
        int2let 0 = 'Z'
        int2let x = chr $ (x - 1) + ord 'A'
        base26  0 = []
        base26  i = let i' = (i `mod` 26)
                        i'' = if i' == 0 then 26 else i'
                    in seq i' (i' : base26 ((i - i'') `div` 26))

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
  where f acc pos = case find (flip subsumes pos) acc of 
                          Just _  -> delete pos acc
                          Nothing -> acc 

reducedUnion :: State -> State -> State
reducedUnion st1 st2 = reduce $ union st1 st2

powerSet :: EditDistance -> [CharacteristicVector]
powerSet n = [(mask, k) | k <- [0..(2 * n + 1)], mask <- [0..(2 ^ k) - 1]]

baseState :: State -> StateDelta
baseState [] = ([], 0)
baseState st = let lowi = lowf st in (map (\(i, e) -> (i - lowi, e)) st, lowi)
  where lowf st = fst $ minimumBy (\a b -> compare (fst a) (fst b)) st

delta1 :: State -> CharacteristicVector -> State
delta1 [] _ = emptyState
delta1 st x = foldl f emptyState st
  where f acc p = reducedUnion acc $ delta editDistance p (x' p x)
        x' (i, e) (b, k) = let k' = min (editDistance - e + 1) (k - i) in
                            ((shiftR b i) .&. ((bit k') - 1), k')

delta'' st = foldl f M.empty (powerSet editDistance)
  where f acc x = M.insert x (baseState (delta1 st x)) acc

generate = generate' M.empty [emptyState, initialState]
  where 
      generate' visited [] = visited
      generate' visited (s:st) = let t = delta'' s
                                     visited' = M.insert s t visited
                                     rest = st `union` (map fst $ M.elems t) \\ (M.keys visited')                                  
                                  in generate' visited' rest

generateStateNames :: [State] -> [String] -> StateNameMap
generateStateNames states names = M.fromList $ zip states names

--main = let states = generate in putStrLn $ printStates (generateStateNames (M.keys states) names) states
main = let states = generate in printStates'' (generateStateNames (M.keys states)  names) states

printStates'' :: StateNameMap -> StateTransitionMap -> IO()
printStates'' names stateTransitions = mapM_ f $ M.assocs stateTransitions
  where f (state, trans) = mapM_ (putStrLn . ((++) (id (name' state))) . ((++) "\t"))
         $ map (\((b, k), (st, delta)) -> (show b) ++ "\t" ++ (show k) ++ "\t" ++ (id $ name' st) ++ "\t" ++ (show delta)) $ M.assocs trans
        name' = fromJust . flip M.lookup names

--printStates' ::  StateTransitionMap -> String
--printStates' = M.foldlWithKey f' ""
--  where f' acc fromState transitions = M.foldlWithKey (g' fromState) acc transitions
--        g' fromState acc charVector (toState, inc) = acc ++ (show (fromState)) ++ " --> " ++ (show charVector) ++ " --> (" ++ (id (toState)) ++ ", " ++ (show inc) ++ ")\n"  

editDistance :: EditDistance
editDistance = 2

emptyState :: State
emptyState = []

initialState :: State
initialState = [(0, 0)]

