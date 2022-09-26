employee_details(a, x, head, 10, 100, a).

employee_details(b, y, head, 6, 50, a).
employee_details(c, y, accountant, 2, 10, b).

employee_details(d, z, head, 3, 20, a).
employee_details(e, z, secretary, 2, 10, d).

department(Name, Department) :-
    employee_details(Name, Department, _, _, _, _).

manager(Name, Manager) :-
    employee_details(Name, _, _, _, _, Manager).

valid_employee(Name) :-
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
    % Also check if the employee is the director ( Is his own manager)
    (   Name = Manager ->  true ; Real_Salary < Real_M_salary).

/** <examples>
?- department(a, Department).
?- department(z, Department).

?- manager(a, Manager).
?- manager(b, Manager).

?- valid_employee(a).
?- valid_employee(b).
?- valid_employee(z).

?- basic_salary(a, Salary).
?- basic_salary(c, Salary).

?- real_salary(b, Real_Salary).
?- real_salary(c, Real_Salary).
*/
