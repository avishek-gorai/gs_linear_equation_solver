PROGRAM gs_linear_equation_solver;

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
    FOR index := 1 TO n-1 DO
    BEGIN
        write(arr[index], blank)
    END;
    write(arr[n])
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

