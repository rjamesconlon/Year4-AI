% delete an element X from list
% This is a recursive function
% Function def:
%     delete_all(<element_to_delete>, <list_to_delete_from>, <Result>).

% Base recursive call, meaning when the list to delete from in exhausted (empty).
delete_all(_, [], []).

% Second rule, this adds the element picked out from the list to the beginning of result
% this is only done if the element is not equal to X, the element to delete
delete_all(X, [H|T], [H|Result]) :-
    (X \= H),
    delete_all(X, T, Result).

% third rule, if the second rule is false, then X must equal H. I added (X = H) as part
% of the clause to prevent the program from going through all possible routes when the second
% clause is false
delete_all(X, [H|T], Result) :-
    (X = H),
    delete_all(X, T, Result).




% replace occurances of element X with Y
% Function def:
%     replace_all(<element_being_replaced>, <element_replacing>, <list_to_replace_in>, <Result>).
%
% this function works similarly to delete_all, however when constructing the list it checks whether
% the element being checked is equal to the element to replace.
% If it is, then the element replacing it is instead appended to the list as the head.
%
%
replace_all(_, _, [], []).

replace_all(X, Y, [H|T], [H|Result]):-
    (H \= X),
    replace_all(X, Y, T, Result).

replace_all(X, Y, [H|T], [Y|Result]) :-
    (H = X),
    replace_all(X, Y, T, Result).
