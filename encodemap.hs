import Data.List
import Data.Maybe

c = 4

-- Given a map with r regions and c colors
-- produce the atom number for region i color j
region_atom r (i, j) = r * i + j

adjacency_atom r (i, j) = r * c + i * r + j

-- Produce list of clauses enforcing that region
-- i has color j drawn from cs if necessary.
colorize r cs (i, '?') = []
colorize r cs (i, j) =
    map color_delta [1..c]
    where
      c0 = 1 + fromJust (findIndex (== j) cs)
      color_delta d | d == c0 = [region_atom r (i, d)]
      color_delta d = [-region_atom r (i, d)]

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
          concat [[[-region_atom r (i, j), -region_atom r (i, k)]
                        | j <- [1..c], j /= k] | k <- [1..c]]

-- Produce list of clauses enforcing that
-- adjacent regions do not have the same color
no_adjacencies r =
    concatMap na' [1..c]
    where
      na' k = 
          concat [[[-adjacency_atom r (i, j),
                    -region_atom r (i, k), -region_atom r (j, k)]
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
  let adjacencies = map (map read . tail . tail) map_elems
  let adjacency_list = concatMap (adjacentize r) $ zip [1..] adjacencies
  let clauses = color_list ++ adjacency_list ++
                (some_color r) ++ (no_two_colors r) ++ (no_adjacencies r)
  putStrLn $ "p cnf " ++ show (r*c + r*r) ++ " " ++ show (length clauses)
  putStr $ unlines (map (unwords . map show . (++ [0])) clauses)
