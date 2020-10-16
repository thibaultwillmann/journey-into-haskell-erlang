% In this exercise you’ll create a a network of processes in a star topology - where a single hub connects to N nodes.
%  • The hub will send M messages (taken from a Messages list) to each of the nodes.
%  • Each node will send the each message it receives back to the hub.
%  • The hub will send a single message from the Messages list to all of the nodes, and then it will wait until receiving it back from all of the nodes before sending the next message in the list.
%  • When there are no more messages to send - the hub will send each of the nodes a closing message telling them to finish so they will exit gracefully.
%    Then the hub will wait until all the nodes send the closing message back to it before closing itself.
%  • The hub will report each message it sends - with the following format: "Hub sent <PID> the message: " followed by the message.
%  • The hub will report each message it receives back - with the following format: "Hub received from <PID> the message: " followed by the message.
%  • The nodes will report each message they receive - with the following format: "<PID> received message: " followed by the message.
%  • When the hub sends a closing message it will report: "Hub sent closing message to <PID>.".
%  • When the hub receives back a closing message it will report: "Hub received closing message from <PID>.".
%  • When a node receives a closing message it will report: "<PID> received closing message.".
%  • Before the hub exits (after receiving back all of the closing messages) it will report with the following message: "Hub has finished.".

-module(hwbonus).
-compile(export_all).

% Write a function start which takes 2 arguments: 
%  1. N - the number of processes.
%  2. Messages - the list of messages.
% The function will create N processes and send them the messages, by the given instructions.

%I assume N is the number of Nodes and current process is the Hub.

start(N, Messages) ->
  NodePIDs = createNodes(N, self(), []),
  hubHandler(NodePIDs, Messages).

%Sends back list of the PIDs of the Nodes created.

createNodes(0, HubPID, NodePIDs) ->
  NodePIDs;
createNodes(N, HubPID, NodePIDs) ->
  NewPID = spawn(hwbonus, nodeHandler, [HubPID]),
  createNodes(N - 1, HubPID, NodePIDs ++ [NewPID]).

%Handler for messages of each node.

nodeHandler(HubPID) ->
  receive
    finish ->
      io:format("~w received closing message.~n", [self()]),
      HubPID ! {finish, self()};
    {message, Msg} ->
      io:format("~w received message: ~p ~n", [self(), Msg]),
      HubPID ! {message, Msg , self()},
      nodeHandler(HubPID)
  end.

%Handler for the list of messages of the hub.

hubHandler(NodePIDs, []) ->
  hubSend(NodePIDs, finish),
  hubReceive(NodePIDs),
  io:format("Hub has finished.~n");
hubHandler(NodePIDs, [Msg | Messages]) ->
  hubSend(NodePIDs, Msg),
  hubReceive(NodePIDs),
  hubHandler(NodePIDs, Messages).

%Sends each node a message.

hubSend([], _) ->
  io:format("");
hubSend([NodePID | NodePIDs], finish) ->
  io:format("Hub sent closing message to ~w. ~n", [NodePID]),
  NodePID ! finish,
  hubSend(NodePIDs, finish);
hubSend([NodePID | NodePIDs], Msg) ->
  io:format("Hub sent ~w the message: ~p ~n", [NodePID, Msg]),
  NodePID ! {message, Msg},
  hubSend(NodePIDs, Msg).

%Receives from each Node a Message.

hubReceive([]) ->
  io:format("");
hubReceive([ReceivedPID | NodePIDs]) ->
  receive
    {message, Msg, ReceivedPID} ->
      io:format("Hub received from ~w the message: ~p ~n", [ReceivedPID, Msg]),
      hubReceive(NodePIDs);
    {finish, ReceivedPID} ->
      io:format("Hub received closing message from ~w ~n", [ReceivedPID]),
      hubReceive(NodePIDs)
  end.
