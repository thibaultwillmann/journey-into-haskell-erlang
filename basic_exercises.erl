-module(hw6).
-compile(export_all).


%1. Implement the function take which takes an integer N and a list Xs and returns the first N elements of the list.

take(0, _)             -> [];
take(_, [])            -> [];
take(N, [Hd | Tl])     -> [Hd | take(N - 1, Tl)].


%2. Implement the function applyList which takes 2 lists - Fs which is a list of functions, and Xs which is a list of arguments, 
%   and returns the list of the results of applying the i’th function in Fs to the i’th element of Xs.
%   If one of the given lists is longer than the other, the function will ignore any element behind the length of the shorter list.

applyList([], _)               -> [];
applyList(_, [])               -> [];
applyList([F | Fs], [Hd | Tl]) -> [F(Hd) | applyList(Fs, Tl)].


%3. To represent binary trees we’ll use the atoms leaf and node. Each tree is a tuple of one of two forms:
%    • {leaf, X} - represents a leaf which has the value X.
%    • {node, L, R} - represents a node which has the left child L and the right child R, where both L and R are also binary trees.

%3.1. Implement the function sumTreeSeq which takes a tree and sums all the values in its leaves sequentially, within a single process.

sumTreeSeq({leaf, X})    -> X;
sumTreeSeq({node, L, R}) -> sumTreeSeq(L) + sumTreeSeq(R).

%3.2. Implement the function sumTreePar which takes a tree and sums all the values in its leaves in parallel, by creating a new process for each node in the tree.
%     When the function is applied to the pattern {node, L, R} the function spawns a new process to compute the sum of L, 
%     and a new process to compute the sum of R, and waits for the results before adding them up.

% Wrapper function calls the Worker with its PID, Worker recursively passes on its PID and waits for results from Child Processes

sumTreeParWorker({leaf, X}, Parent) -> Parent ! X;
sumTreeParWorker({node, L, R}, Parent) ->
      spawn(hw6, sumTreeParWorker, [L, self()]),
      spawn(hw6, sumTreeParWorker, [R, self()]),
      receive LSum ->
          receive RSum ->
              Parent ! (LSum + RSum)
          end
      end.

sumTreePar(Tree) ->
      sumTreeParWorker(Tree, self()),
      receive Sum -> Sum end.


%4. Simulate a bank account by creating a process which manages the "balance" of the account (which represents how much money the account holds).
%   The process can receive and handle 2 kinds of messages:
%    • {deposit, Amount} - Add the given amount to the balance of the account.
%    • {withdraw, Amount} - Subtract the given amount from the balance of the account.
%   To initialze the account we’ll use a function createAccount which has no arguments and returns the process ID of the process which handles the account.
%   The createAccount function creates an account with an initial balance of 0.
%   After each operation (create, deposit, withdraw) the process will print to the standard output the message: "The balance is: " followed by the current account balance.
%   There is no need to worry about overdraft (the balance could become negative).

% createAccount creates Process that waits for messages indefinetly

account(Balance) ->
      receive
         {deposit, Amount} ->
             io:format("The balance is: ~w ~n", [Balance + Amount]),
             account(Balance + Amount);
         {withdraw, Amount} ->
             io:format("The balance is: ~w ~n", [Balance - Amount]),
             account(Balance - Amount)
      end.

createAccount() ->
      io:format("The balance is: 0 ~n"),
      spawn(hw6, account, [0]).
