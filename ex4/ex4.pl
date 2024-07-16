/*
 * **********************************************
 * Printing result depth
 *
 * You can enlarge it, if needed.
 * **********************************************
 */
maximum_printing_depth(100).

:- current_prolog_flag(toplevel_print_options, A),
   (select(max_depth(_), A, B), ! ; A = B),
   maximum_printing_depth(MPD),
   set_prolog_flag(toplevel_print_options, [max_depth(MPD)|B]).

% Define the edges of the graph
edge(a, b).
edge(a, c).
edge(c, b).
edge(c, a). 

% Signature: path(Node1, Node2, Path)/3
% Purpose: Path is a path, denoted by a list of nodes, from Node1 to Node2.
path(Node1, Node2, [Node1, Node2]):- edge(Node1, Node2). % base case
path(Node1, Node2, [Node1|Paths]):- edge(Node1, OtherNode), path(OtherNode, Node2, Paths).


% Signature: cycle(Node, Cycle)/2
% Purpose: Cycle is a cyclic path, denoted a list of nodes, from Node to Node.
cycle(Node, Cycle) :- path(Node, Node, Cycle).


% Signature: nodes(Nodes)/1
% Purpose: Nodes are the nodes in the graph


% Signature: reverse(Graph1, Graph2)/2
% Purpose: The edges in Graph1 are reversed in Graph2

% Base case: The reverse of an empty graph is an empty graph.
reverse([], []).
% Recursive case: Reverse the edge and process the rest of the graph.
reverse([[X, Y] | Rest], [[Y, X] | ReversedRest]) :-
    reverse(Rest, ReversedRest).


% Define Church numerals
natural_number(zero).
natural_number(s(X)) :- natural_number(X).

% Signature: degree(Node, Graph, Degree)/3
% Purpose: Degree is the out-degree of Node, denoted by a Church number

% Base case: The out-degree of a node in an empty graph is zero.
degree(_, [], zero).

% Recursive case: Count the occurrences of Node as the first element in each pair (out-degree) and accumulate the degree.
degree(Node, [[Node, _]|Rest], s(Degree)) :-
    degree(Node, Rest, Degree).
degree(Node, [[Other1, _]|Rest], Degree) :-
    Node \= Other1,
    degree(Node, Rest, Degree).


% Signature: spanning_tree(Tree)/1
% Purpose: Tree is a spanning tree of the graph (as defined by the edge predicates), denoted by the pre-order list of nodes in the tree.





