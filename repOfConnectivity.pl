connected(a,b,2).
connected(a,c,1).

connected(b,e,4).
connected(b,g,5).

connected(e,g,1).

connected(c,d,1).
connected(c,x,3).

connected(x,g,1).

goal(g).

can_reach_goal(Node) :- connected(Node, g, Val).
can_reach_goal(Node) :- goal(Node).
can_reach_goal(Node) :-
    connected(Node, Node2, Val),
    can_reach_goal(Node2).

find_path(Start, End, End) :- connected(Start, End, _).
find_path(Start, End, [Node|Path]) :-
        connected(Start, Node, _),
        find_path(Node, End, Path).
