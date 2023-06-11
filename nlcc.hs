
-- Index type created to identify index of an element in a sub list
-- Element type created to store the location of a number along with index as a list of tuples
type Index = (Int,Int)
type Element = [(Index,Int)]


-- beginning function with 2 parameters .First parameter is a list of lists containing 0 and 1 and second parameter is 
-- either 0 or 1 and the function returns the length of largest connected path of the input number
nlcc :: [[Int]] -> Int -> Int
nlcc l v = length [fst c | c <- connectedcomponent [((p,q),n) | (r,p) <- zip l [0..], (n,q) <- zip r [0..]]  v]
 
-- (r,p) <- zip l [0..] creates a list of pairs, where each pair consists of a row of l and its corresponding index in the outer
--, list which is  the row number.
--(r,p) <- zip l [0..], (n,q) <- zip r [0..]creates a list of pairs, where each pair consists of an element of l and its 
--,corresponding index in the grid (i.e. the row and column numbers).
--((p,q),n) | (r,p) <- zip l [0..], (n,q) <- zip r [0..]  creates a list of pairs, where each pair consists of a cell coordinate
--, (p,q) and its corresponding value n.
--connectedcomponent takes this list of cell-value pairs and returns a list of all the cells in the largest connected component
-- of cells with value v.
--[fst c | c <- largestComponent ..., v] extracts the coordinates of the cells in the largest connected component with value v.
-- ((p,q),n) is a tuple denoting an Cells n with its Cell value (p,q)
-- connectedcomponent takes as argument a list of tuples and the value v and returns the largest connected component containing value v
-- The fst function is extracts the first Cells of each tuple which is the Cell of the connected component
-- length returns the length of the largest connected component with Cells value v

-- The filterCells function returns the list of Cells that either contains the given Cell or have the same integer value as the given Cell.
--The filterCells function takes an integer v, a list of indices and their corresponding values present, and an index-value pair x, 
--and returns a modified present list with x added to it if the value of x matches v and x is not already present in present.

filterCells :: Int -> Element -> (Index, Int) -> Element
filterCells v present p =
  if snd p /= v || p `elem` present then present else present ++ [p]


--v: the input number
--present: a list of index-value pairs that have already been filtered and match the integer value v.
--p: a tuple of an index-value pair that we want to check if it matches the integer value v.

--The function Check if the value of x does not match v or if x is already present in present. If either of these conditions is true,
--return the original present list unchanged.
--If the value of x matches v and x is not already present in present, add x to present and return the modified list.


--The function takes two lists as argument and returns the largest list of them comparing their length
filterLists :: [x] -> [x] -> [x]
filterLists p q = if length p > length q then p else q

connectedcomponent cells color =
  let components = map (foldl (filterCells color) [] . neighbour cells) cells
  in foldl filterLists [] components

--connectedcomponent function takes a list of Cells and an integer value. It returns the largest 
-- connected component in the list of Cells that contains the given integer value. It does so by mapping the neighbour 
-- function over the list of Cells and applying the filterLists function to the resulting list of components.

--neighbour function takes a list of Cells and a Cell with an integer value. It returns the connected component that contains the given Cell.
--Firstly, it is checked if the integer in argument is -1 ,if yes an empty list is returned.
--If not,present is initilized with the index and integer value of the current cell.

neighbour :: Element -> (Index,Int) -> Element
neighbour _ (_,-1) = []
neighbour element ((p,q), v) = let present = ((p,q), v)
                                   north = extract element (p, q -1)
                                   south = extract element (p, q + 1)
                                   west = extract element (p -1, q)
                                   east = extract element (p + 1, q)
                                   latest = removeComponent element ((p,q), v)
                                   nearest ((e,f),g) v = if (e >= 0 && e < 100 && f >= 0 && f < 100) && g == v then ((e,f),g) else ((-1,-1),-1) 
                                   makeComponent = neighbour latest
                                in  present : concat [makeComponent (nearest north v), makeComponent (nearest south v), makeComponent (nearest west v), makeComponent (nearest east v)]

-- north,south,east and west stores the immediate neighbours of the Cell (p,q),obtained by adding (+/-)1 to p and q indices value
-- neighbour is used to  recursively find the Cells that are connected to the given Cell with the same integer value,
--checking all Cells in north,south,east and west of the Cell.

--The removeComponent function takes an Element list and a tuple (Index,Int) as input. It removes the element from the 
--Element list whose index matches the given index in the tuple.

--The nearest function takes a cell ((e,f),g) and an integer v and returns the cell if it has the same value as v and its indices 
--are within the bounds of the 100x100 grid, and otherwise returns a dummy cell ((-1,-1),-1).

--The neighbour function initializes makeComponent to a recursive call of neighbour on the latest Element list obtained by removing the 
--current element, and uses concat to concatenate the lists returned by four recursive calls of makeComponent on the four nearest elements.


removeComponent :: Element -> (Index,Int) -> Element
removeComponent [] _ = []
removeComponent (((e,f),g):xs) ((p,q),v) = if e /= p || f /= q then  ((e,f),g) : removeComponent xs ((p,q),v) else removeComponent xs ((p,q),v)

--The function first checks if the input list is empty, in which case it returns an empty list. Otherwise, it checks the first element of
-- the list, which is a tuple of the form ((e,f),g) where (e,f) represents the index of the element and g is its value. If the index of 
--this element matches the given index in the tuple, the function recursively calls itself with the remaining list and the same index tuple. Otherwise, it prepends the first element of the list to the result obtained by recursively calling itself with the remaining list and the same index tuple.
--By doing this, the function removes the first occurrence of the given index in the input list and returns the modified list.


-- extract function searches a list of elements for an element at a specific index.
 -- a tuple containing the position and value of element is returned.
 
extract :: Element -> Index -> (Index,Int)
extract [] _ = ((-1,-1), -1)
extract (((p,q),v):xs) (d,e) = if p == d && q == e then ((p,q),v) else extract xs (d,e)
--First element of a list is serached and if match not found ,subsequent values are serached for the element,by calling
-- the function recursively.If no match found ,((-1,-1), -1) is returned.
