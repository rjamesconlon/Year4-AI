
% Cars
car(3, ford, 5000).
car(2, opel, 6000).
car(5, toyota, 1000).
car(2, ford, 2000).

% People who have a car
has(joe, car(3, ford, 5000)).
has(joe, car(2, opel, 6000)).
has(mick, car(5, toyota, 1000)).
has(mick, car(2, ford, 2000)).

% check if the type exists
car(Type) :-
    car(_, Type, _).

% check what cars are less than Amount
car_less_than(Who, Amount) :-
    has(Who, car(_, _, Cost)), Cost < Amount.

% Check who owns what cars
what_car(Name, Car) :-
    has(Name, Car).


/* Queries */

/** <examples>

?- car(toyota).
?- car(renault).

?- car_less_than(3500).
?- car_less_than(500).

What cars does joe own ?
?- what_car(joe, Car).

*/
