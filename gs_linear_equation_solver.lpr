PROGRAM gs_linear_equation_solver;

(* gs_linear_equation_solver.pas -- Solves system of linear equation using Gauss-Seidel method.
   Copyright (C) 2025 Avishek Gorai <avishekgorai@myyahoo.com>

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)


USES
    gs_linear_equation_solver_functions;

CONST
    blank = ' ';

VAR
    number_of_variables : maximum_variables;
    coeffifient_matrix  : matrix;
    answer, constants   : standard_array;
    dd_result           : diagonal_dominance_result;

PROCEDURE print_answers(arr : standard_array; n : maximum_variables);

VAR
    index : maximum_variables;

BEGIN
    FOR index := 1 TO n DO
    BEGIN
        write(arr[index], blank)
    END;
    writeln
END;

BEGIN
    write('Enter number of variables: ');
    read(number_of_variables);

    writeln('Enter coefficients:-');
    read_coefficients(coeffifient_matrix, number_of_variables);

    writeln('Enter constants:-');
    read_constants(constants, number_of_variables);

    dd_result := diagonal_dominance(coeffifient_matrix, number_of_variables);

    IF dd_result.test = true THEN
      BEGIN
        answer := calculate(coeffifient_matrix, constants, number_of_variables);
        print_answers(answer, number_of_variables)
      END
    ELSE
      BEGIN
          writeln('Diagonal dominance not satisfied by equation ', dd_result.equation_number)
      END
END.
