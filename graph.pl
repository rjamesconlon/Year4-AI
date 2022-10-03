parent(pam, bob).
parent(tom, bob).
parent(tom, liz).

parent(bob, anne).
parent(bob, pat).

parent(pat, jim).

female(pam).
female(liz).
female(anne).

male(tom).
male(bob).

sibling(X, Y) :-
    % Check if X and Y are siblings (Do they have a parent in common?)
    parent(Z, X),
    parent(Z, Y),
    (X \= Y).

sister(X, Y) :-
    % Check if X is a sister (Are they female, do they have a sibling?)
    female(X),
    sibling(X, Y).
 
aunt(X, Y) :-
    % Check if X is an aunt (Do they have a sibling? Does the sibling have a child?)
    sister(X, Y),
    parent(Y, _).