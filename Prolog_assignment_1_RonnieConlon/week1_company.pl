employee_details(mick, main, head, 10, 100, mick).

employee_details(joe, finance, head, 6, 50, mick).
employee_details(michael, finance, accountant, 2, 10, joe).

employee_details(anne, hr, head, 3, 20, mick).
employee_details(liz, hr, secretary, 2, 10, anne).

% Search which employee details contain Name, Department
department(Name, Department) :-
    employee_details(Name, Department, _, _, _, _).

% Get the manager name of employee, or employees of manager
manager(Name, Manager) :-
    employee_details(Name, _, _, _, _, Manager).


valid_employee(Name) :-
    % Recursive function,
    % Check if employee exists and check if manager is equal to employee
    % If true then they're the director, otherwise go up the chain until true
    employee_details(Name, _, _, _, _, Manager),
    ((Manager = Name) -> true ; valid_employee(Manager)).

basic_salary(Name, Salary) :-
    employee_details(Name, _, _, _, Salary, _).

real_salary(Name, Real_Salary) :-
    % Get Employee and their manager's salary ( Salary , M_Salary )
    employee_details(Name, _, _, Years, Salary, Manager),
    employee_details(Manager, _, _, M_years, M_salary, _),
    % Check if the employee and manager are working over 5 years, if so, then add 5000 to salary
    %And assign to Real_Salary, and Real_M_Salary
    (   Years > 5 ->  Real_Salary is Salary + 5000 ; Real_Salary is Salary ),
    (   M_years > 5 ->  Real_M_salary is M_salary + 5000 ; Real_M_salary is M_salary ),
    % Check if employee salary is less than manager salary ( Must have less than manager )
    % Also check if the employee is the director ( Is his own manager
    (   Name = Manager ; Real_Salary < Real_M_salary).

/** <examples>
?- department(mick, Department).
?- department(anne, Department).

?- manager(mick, Manager).
?- manager(liz, Manager).

?- valid_employee(mick).
?- valid_employee(liz).
?- valid_employee(john).

?- basic_salary(joe, Salary).
?- basic_salary(mick, Salary).

?- real_salary(joe, Real_Salary).
?- real_salary(mick, Real_Salary).
*/
