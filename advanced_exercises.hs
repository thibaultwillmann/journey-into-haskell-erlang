--1.a) Implement the function replaceElement which takes a pair (i,x) (where i is an index of type Int, and x is a polymorphic item), 
--     and a polymorphic list - and sets the i’th element of the list to be x.
--     If i is negative or bigger than the length of the list - it just returns the list without changing it.
replaceElement :: (Int, a) -> [a] -> [a]
replaceElement _       []       = []
replaceElement (i, el) (x : xs) = if i == 0 then el : replaceElement (i - 1, el) xs else x : replaceElement (i - 1, el) xs


--1.b) Implement the function replaceElements which takes a list of pairs of (i,x) (where i is an Int), and a list xs, 
--     and for each pair (i,x) it sets the i’th element in xs to be x. If a pair contains in its 1st component a negative index or 
--     an index bigger than the length of xs - ignore this pair. If the pairs list contains more than one pairs with the same key 
--     (i.e - [(1,'a'),(1,'b')]) - the last occurrence of the key will be the one that holds.
replaceElements :: [(Int, a)] -> [a] -> [a]
replaceElements []       y = y
replaceElements (x : xs) y = replaceElements xs (replaceElement x y)


--2. In this section we’ll use a list of pairs to represent a simplified "database" where each item has a non-unique "key" (which is given as a String).
--   The database should be polymorphic - the implemented functions should work for any type of items (while the type of keys must be String).


--2.a) Implement the addItem function which takes a pair of (key,item) (where key is a String and item is polymorphic), and a list of (key,item) pairs - 
--     and adds the pair to beginning of the list.
addItem :: (String, a) -> [(String, a)] -> [(String, a)]
addItem pair pairs = pair : pairs


--2.b) Implement the subsetByKey function which takes a key (given as a String), and a list of (key,item) pairs - 
--     and returns all the items in the list, which have the given key.
subsetByKey :: String -> [(String, a)] -> [a]
subsetByKey key pairs = map snd $ filter (\pair -> (fst pair) == key) pairs


--2.c) Implement the subsetByKeys function which takes a list keys of keys, and a list xs of (key,item) pairs - 
--     and returns all the items in xs, which have one of the keys in keys.
--     You may assume the keys in the keys do not repeat (so you don’t have to worry about cases such as subsetByKeys ["a", "b", "a"]).
subsetByKeys :: [String] -> [(String, a)] -> [a]
subsetByKeys keys pairs = (concat . map (\key -> subsetByKey key pairs)) keys


--2.d) Implement the getKeys function which takes a list xs of (key,item) pairs and returns a list of all the keys of xs, where each key appears only once!.
--     (note: the order of the elements in the result doesn’t matter! - as long as all the keys are in the list and there are no duplicates)
getKeys :: [(String, a)] -> [String]
getKeys keys = foldl (\unique key -> if key `elem` unique then unique else unique ++ [key]) [] $ map fst keys


--2.e) Implement the groupByKeys function which takes a list xs of (key,item) pairs and returns a list of (key,items) pairs 
--     where each pair contains a key and a list of all the items in xs which have this key. Each key must appear only once in the result.
--     (note: the order of the elements in the result doesn’t matter!)
groupByKeys :: [(String, a)] -> [(String, [a])]
groupByKeys keys = map (\key -> (key, subsetByKey key keys)) $ getKeys keys


--3. In this section we’ll use a list of lists to represent a matrix, where each row in the matrix will be represented by a list.
--   All the functions in this section, except addMatrices (subsection e), should be polymorphic - they should work for any type of elements.


--3.a) Implement the createMatrix function which takes 2 positive Ints - m and n, and a list xs, and creates a matrix of size m × n (m rows and n columns).
--     You may assume the list xs will always be of the size m · n
createMatrix :: Int -> Int -> [a] -> [[a]]
createMatrix 0 _ _  = []
createMatrix m n xs = take n xs : createMatrix (m - 1) n (drop n xs)


--3.b) Implement the getElementInCell function which takes 2 Ints m and n and a matrix - and returns the element in the m’th row and n’th column of the matrix.
--     You may assume m and n will always be within the boundaries of the matrix.
getElementInCell :: Int -> Int -> [[a]] -> a
getElementInCell m n xss = (xss !! m) !! n


--3.c) Implement the appendH function which takes 2 matrices and appends them horizon- tally. You may assume both matrices have the same number of rows.
appendH :: [[a]] -> [[a]] -> [[a]]
appendH xss yss = zipWith (++) xss yss


--3.d) Implement the appendV function which takes 2 matrices and appends them vertically. You may assume both matrices have the same number of columns.
appendV :: [[a]] -> [[a]] -> [[a]]
appendV xss yss =  xss ++ yss


--3.e) Implement the addMatrices function which takes 2 matrices of Ints and returns the result of their matrix addition (adding each 2 corresponding values).
--     You may assume both matrices have the same dimensions.
addMatrices :: [[Int]] -> [[Int]] -> [[Int]]
addMatrices xss yss = zipWith (zipWith (+)) xss yss
