--1. The following BNF represents arithmetic expressions which have variables in them:

data Expr
    = Const Value     -- a constant number.
    | Add Expr Expr   -- addition of expressions.
    | Mul Expr Expr   -- multiplication of expressions.
    | Sub Expr Expr   -- subtraction of expressions.
    | Div Expr Expr   -- division of expressions.
    | Var Variable    -- a variable.

-- where:

type Variable = String
type Value = Float

-- To evaluate the BNF - we’ll also use the following type synonyms:

type Dictionary = [(Variable, Value)]
type EvalError = [Variable]
type EvalResult = Either EvalError Value

-- The Expr expression may contain variables.
-- In order to evaluate the variables we will look for the value of a variable in the Dictionary (which maps variables to values).
-- If we try to evaluate an expression which contains variables which aren’t in the Dictionary - we’ll get an EvalError. 
-- The EvalError will contain all the variables that appear in the expression but not in the Dictionary.


--1.a) Write a function display which takes an expression and returns a string which represents the expression.
--     display needs to follow the following rules:
--      • Addition, multiplication, subtraction, and division are displayed using the infix +, *, -, / - respectively.
--      • Arithmetic operations (addition, multiplication, subtraction, and division) will always appear within parentheses.
--      • A Variable is always displayed as it is.
--      • To display a Value - simply use the built-in function show on it.

display :: Expr -> String
display (Const m) = show m
display (Add l r) = "(" ++ display l ++ "+" ++ display r ++ ")"
display (Mul l r) = "(" ++ display l ++ "*" ++ display r ++ ")"
display (Sub l r) = "(" ++ display l ++ "-" ++ display r ++ ")"
display (Div l r) = "(" ++ display l ++ "/" ++ display r ++ ")"
display (Var m)   = m


--1.b) Write a function eval. This function will take a dictionary and an expression, and will evaluate the expression by the following rules:
--      • Valid values are returned by wrapping them with the Right constructor. Evaluation errors are returned by wrapping them with the Left constructor.
--      • A Value is simply returned without further evaluation (since it is already a value).
--      • The Add, Mul, Sub, and Div expressions are evaluated by doing addition, multipli- cation, subtraction, and division (respectively) of the values of their subexpressions.
--      • A Variable is evaluated by looking for its value in the dictionary. You may assume the variable only appear once in the dictionary.
--      • If there is at least 1 variable in the expression which doesn’t have a value in the dictionary - instead of a Value the eval function will return an EvalError, 
--        which will contain all the undefined variables in the expression.
--      • The order of the variables in the EvalError list doesn’t matter.

eval :: Dictionary -> Expr -> EvalResult
eval x (Const m) = (Right m)
eval x (Add l r) = op (+) (eval x l) (eval x r)
eval x (Mul l r) = op (*) (eval x l) (eval x r)
eval x (Sub l r) = op (-) (eval x l) (eval x r)
eval x (Div l r) = op (/) (eval x l) (eval x r)
eval x (Var m)   = find x m

op :: (Value -> Value -> Value) -> EvalResult -> EvalResult -> EvalResult
op f (Left x)  (Left y)  = (Left (x++y))
op f (Left x)  (Right y) = (Left x)
op f (Right x) (Left y)  = (Left y)
op f (Right x) (Right y) = (Right (f x y))

find :: Dictionary -> Variable -> EvalResult
find []                  x = (Left [x])
find ((var, val) : dict) x = if var == x then (Right val) else find dict x


--2. The questions in this section are about the following polymorphic tree data type:

data Tree a b = Leaf b | Node a (Tree a b) (Tree a b) deriving (Show)


--2.a) Create a function reverseTree which takes a tree and reverses it (so the output will be its mirror reflection).

reverseTree :: Tree a b -> Tree a b
reverseTree (Leaf y) = Leaf y
reverseTree (Node x l r) = Node x (reverseTree r) (reverseTree l)


--2.b) Create a function isSubtree which takes 2 trees of type Tree Int Char and returns True if the 1st tree is a subtree of the 2nd tree and False otherwise.
--     Note: a tree is a subtree of itself.

isSubtree :: Tree Int Char -> Tree Int Char -> Bool
isSubtree (Leaf x)       (Leaf y)       = x == y
isSubtree (Node _ _ _)   (Leaf _)       = False
isSubtree (Leaf x)       (Node _ l r)   = isSubtree (Leaf x) l || isSubtree (Leaf x) r
isSubtree (Node x xl xr) (Node y yl yr) = if x == y
  then compareTrees xl yl && compareTrees xr yr
  else isSubtree (Node x xl xr) yl || isSubtree (Node x xl xr) yr

compareTrees :: Tree Int Char -> Tree Int Char -> Bool
compareTrees (Leaf x)       (Leaf y)       = x == y
compareTrees (Node x xl xr) (Node y yl yr) = x == y && compareTrees xl yl && compareTrees xr yr
compareTrees _              _              = False


--3. The following data type represents tree where each node can have many children:

data MTree a = MTree a [MTree a] deriving (Show)

-- In this kind of tree, the leaves are nodes that have an empty children list.


--3.a) Create a function sumMTree which takes a tree of type MTree Int and computes the sum of all of its nodes.

sumMTree :: MTree Int -> Int
sumMTree (MTree x [])  = x
sumMTree (MTree x y)   = x + sum (map (sumMTree) y)


--3.b) Create a function grow that takes an MTree a and replace each leaf in the tree with a copy of the original tree.

grow :: MTree a -> MTree a
grow (MTree x y) = grow' (MTree x y) (MTree x y)

grow' :: MTree a -> MTree a -> MTree a
grow' (MTree x []) (MTree y z) = MTree y z
grow' (MTree x y) z = MTree x $ map (\a -> grow' a z) y
