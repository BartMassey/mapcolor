import Data.List
import Data.Maybe

-- Hardcoded number of map colors
c = 4

-- Given a map with r regions
-- produce the atom number for "region i is color j"
region_atom r (i, j) = show $ r * i + j
--region_atom r (i, j) = "r(" ++ show i ++ "," ++ show j ++ ")"

-- Given a map with r regions
-- produce the atom number for "region i is adjacent to region j"
adjacency_atom r (i, j) = show $ r * c + i * r + j
--adjacency_atom r (i, j) = "a(" ++ show i ++ "," ++ show j ++ ")"

-- Negate an atom
neg x = "-" ++ x

-- Fix a clause up for printing
fix_clause c = c ++ ["0"]
--fix_clause c = c

-- Produce list of clauses enforcing that region
-- i has color j drawn from cs if necessary.
colorize r cs (i, '?') = []
colorize r cs (i, j) =
    map color_delta [1..c]
    where
      c0 = 1 + fromJust (findIndex (== j) cs)
      color_delta d | d == c0 = [region_atom r (i, d)]
      color_delta d = [neg (region_atom r (i, d))]

-- Produce list of clauses enforcing that region
-- i is adjacent to regions in js
adjacentize r (i, js) = [[adjacency_atom r (i, j)] | j <- js]

-- Produce list of clauses enforcing that
-- every region has some color
some_color r =
    [[region_atom r (i, j) | j <- [1..c]] | i <- [1..r]]

-- Produce list of clauses enforcing that
-- no region has more than one color
no_two_colors r =
    concatMap ntc' [1..r]
    where
      ntc' i = 
          concat [[[neg (region_atom r (i, j)), neg (region_atom r (i, k))]
                        | j <- [1..c], j /= k] | k <- [1..c]]

-- Produce list of clauses enforcing that
-- adjacent regions do not have the same color
no_adjacencies r =
    concatMap na' [1..c]
    where
      na' k = 
          concat [[[neg (adjacency_atom r (i, j)),
                    neg (region_atom r (i, k)), neg (region_atom r (j, k))]
                       | i <- [1..r], i /= j] | j <- [1..r]]

main = do
  map_data <- getContents
  let map_elems = map words $ lines $ map_data
  let indices = map (read . head) map_elems
  if (indices /= [1..length map_elems])
      then error "unsorted map data"
      else return ()
  let r = length indices
  let colors = map (\(_ : [c] : _) -> c) map_elems
  let color_names = filter (/= '?') $ nub colors
  let color_list = concatMap (colorize r color_names) $ zip [1..] colors
  let adjacencies = map (map read . tail . tail) map_elems :: [[Int]]
  let adjacency_list = concatMap (adjacentize r) $ zip [1..] adjacencies
  let clauses = color_list ++ adjacency_list ++
                (some_color r) ++ (no_two_colors r) ++ (no_adjacencies r)
  putStrLn $ "p cnf " ++ show (r*c + r*r) ++ " " ++ show (length clauses)
  putStr $ unlines $ map (unwords . fix_clause) clauses
