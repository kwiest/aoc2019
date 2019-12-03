module Day2
  (
    run
  ) where

input :: [Int]
input = [ 1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,6,19,1,9,19,23,2,23,10,27,1,27,5,31,1,31,6,35,1,6,35,39,2,39,13,43,1,9,43,47,2,9,47,51,1,51,6,55,2,55,10,59,1,59,5,63,2,10,63,67,2,9,67,71,1,71,5,75,2,10,75,79,1,79,6,83,2,10,83,87,1,5,87,91,2,9,91,95,1,95,5,99,1,99,2,103,1,103,13,0,99,2,14,0,0
        ]

getOp :: Int -> Maybe (Int -> Int -> Int)
getOp 1 = Just (+)
getOp 2 = Just (*)
getOp _ = Nothing

replaceAt :: Int -> Int -> [Int] -> [Int]
replaceAt i v xs =
  f ++ (v : tail s)
    where (f, s) = splitAt i xs

program :: Int -> [Int] -> Maybe [Int]
program ptr xs = case (xs !! ptr) of
             99 -> Just xs
             n -> do
               op <- getOp n
               program (ptr + 4) (replaceAt thd (op fst snd) xs)
                 where fst = xs !! (xs !! (ptr + 1))
                       snd = xs !! (xs !! (ptr + 2))
                       thd = (xs !! (ptr + 3))

initialize :: Int -> Int -> [Int] -> [Int]
initialize n v xs = replaceAt 2 v $ replaceAt 1 n xs

bruteForce :: [(Int, Int, [Int])]
bruteForce = [(n, v, (initialize n v input)) | n <- [0..99], v <- [0..99]]

run :: IO ()
run = do
  -- Part 1
  print $ head <$> (program 0 $ initialize 12 2 input)

  -- Part 2
  let desired = 19690720
  case filter (\(_, _, l) -> (head <$> (program 0 l)) == Just desired) bruteForce of
    ((n, v, _):_) -> print $ 100 * n + v
    _             -> print "Can't find anything"

