-- In this exercise we’ll implement the Huffman coding compression algorithm.
-- The algorithm converts a string into a sequence of bits which take up much less space (in bits) than the original string.
-- https://en.wikipedia.org/wiki/Huffman_coding
-- https://www.youtube.com/watch?v=umTbivyJoiI


-- We’ll use the following data types:

data Bit         = O | I deriving (Eq, Show)
data HuffmanTree = Leaf Char Int | Node Int HuffmanTree HuffmanTree deriving (Eq, Show)


-- And the following type synonyms:

type Code       = [Bit]
type FreqTable  = [(Char, Int)]
type Dictionary = [(Char, Code)]
type Encoder    = Char -> Code      --this is a function


-- encode :: String -> (HuffmanTree, Code) which takes a string and returns a pair which contains:
--  1. An Huffman tree which could be used to decode compressed codes. 
--  2. A compressed code which represents the string.
-- decode :: HuffmanTree -> Code -> String which takes an Huffman tree and a compressed code and translates it back into a string by using the tree as a decoder.


--1. Create frequencies table:


--1.a) Create a function which takes a character and a frequencies table and adds 1 to the item in the frequencies table which corresponds to this character, 
--     if it exists; otherwise - it adds a new item to the frequencies table with this character in the 1st component and 1 in the 2nd component. 
--     You may assume there is only 1 item in the list which corresponds to the given character.

insert :: Char -> FreqTable -> FreqTable
insert c []            = [(c, 1)]
insert c ((x, y) : xs) = if   x == c
                         then (x, y + 1) : xs
                         else (x, y)     : insert c xs


--1.b) Create a function count. The function will take a list and return a frequencies table where for each element of the list there is a pair 
--     in which the first component is this element and the second component is the number of times this element occurrs in the list.
--     The order of the items in the list doesn’t matter, but for each character in the input string there should be only 1 item within the table 
--     that counts the number of occurrences of that character in the string. (hint: you can use the insert function you’ve just created)

count :: String -> FreqTable
count = foldr insert []


--2. Convert a frequencies table into a list of Huffman leaves: Create a function initLeaves. 
--   The function will initialize a list of Huffman trees by taking a frequencies table and turning
--   each pair of (character, frequency) into a leaf of an Huffman tree. 

initLeaves :: FreqTable -> [HuffmanTree]
initLeaves = map (\(x, y) -> Leaf x y)


--3. Build an Huffman tree from a list of Huffman leaves: Create a function buildTree. 
--   The function will take a list of Huffman trees and recursively merge them into a single Huffman tree by picking at each step 
--   the 2 trees which have the smallest value within their root and replace them with a Node which contains the sum of their values and 
--   has both of them as children. The order of the leaves doesn’t matter, but at the end result leaves with small numeric values 
--   (which correspond to rare characters) should have a long path from the root, and leaves with large numeric values (which correspond to common characters) 
--   should have a short path from the root. You may assume the input list is non-empty and contains at least 2 leaves.

--in each recursion step, the trees are sorted using quicksort and the two smallest trees merged until one tree left

buildTree :: [HuffmanTree] -> HuffmanTree
buildTree [x] = x
buildTree xs  = buildTree $ merge $ quickTreeSort xs

quickTreeSort :: [HuffmanTree] -> [HuffmanTree]
quickTreeSort []       = []
quickTreeSort (x : xs) = quickTreeSort (filter (\y -> getValue y <= getValue x) xs) ++ [x] ++ quickTreeSort (filter (\y -> getValue y > getValue x) xs)

merge :: [HuffmanTree] -> [HuffmanTree]
merge (x : y : xs) = (Node (getValue x + getValue y) x y) : xs

getValue :: HuffmanTree -> Int
getValue (Node x _ _) = x
getValue (Leaf _ x)   = x


--4. Create dictionary from an Huffman tree: Create a function createDictionary which converts a HuffmanTree into a Dictionary.
--   The Dictionary will hold for each character which appears in the leaves of the tree the path from the root to that leaf.
--   The path will be given as a list of Bits - O for a left child, and I for a right child. (hint: see the paths function in class exercise 5)

createDictionary :: HuffmanTree -> Dictionary
createDictionary (Leaf x _)   = [(x, [])]
createDictionary (Node _ l r) = let writeO = (\(x, y) -> (x, O : y))
                                    writeI = (\(x, y) -> (x, I : y))
                                in map writeO (createDictionary l) ++ map writeI (createDictionary r)


--5. Create encoder from dictionary: Create a function createEncoder which turns a dictionary into an encoder.
--   The encoder is a function which takes a Char as input, and if it has a corresponding code in the Dictionary it will returns that code; 
--   otherwise - it will return the empty list.

createEncoder :: Dictionary -> Encoder
createEncoder []            _ = []
createEncoder ((x, y) : xs) c = if x == c then y else createEncoder xs c


--6. Encode: Create a function encode which when given a String returns both the HuffmanTree derived from it and the Code which represents it.
--   You may assume the string contains at least 2 different characters.
--   (hint: use all the functions you have created so far, and maybe some other helper functions)

encode :: String -> (HuffmanTree, Code)
encode xs = let tree      = buildTree $ initLeaves $ count xs
                directory = createDictionary tree
                code      = concat $ map (createEncoder directory) xs
            in  (tree, code)


--7. Decode: Create a function decode which takes a HuffmanTree and a Code and decodes by using the HuffmanTree the Code into a String.
--   It does so by taking a path from the root to a leaf in the HuffmanTree according to the bits in the Code, going left for each O and right for each I, 
--   until it reaches a leaf - then it returns the character at that leaf and starts again traversing from the root of the tree with the rest of the code 
--   until it reaches the end of the code. It should obey the following law: uncurry decode (encode s) == s

decode :: HuffmanTree -> Code -> String
decode x y = decode' x x y

--this function walks through the tree following the Code and returns the chars at the leafs

decode' :: HuffmanTree -> HuffmanTree -> Code -> String
decode' _    (Leaf c _)   []       = [c]
decode' tree (Leaf c _)   xs       = c : decode' tree tree xs
decode' tree (Node _ l r) (x : xs) = if x == O then decode' tree l xs else decode' tree r xs
