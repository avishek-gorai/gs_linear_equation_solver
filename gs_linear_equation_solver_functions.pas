UNIT gs_linear_equation_solver_functions;

INTERFACE
    CONST
        maximum_variables = 30;

    TYPE
        matrix = ARRAY [1 .. maximum_variables, 1 .. maximum_variables] OF real;
        natural_number = 1 .. maxint;
        standard_array = ARRAY [1 .. maximum_variables] OF real;
        diagonal_dominance_result = RECORD
            equation_number : natural_number;
            test : boolean;
        END;

    FUNCTION diagonal_dominance(coefficient_matrix : matrix) : diagonal_dominance_result;

    FUNCTION calculate(coefficient_matrix : matrix; constants : standard_array) : standard_array;

IMPLEMENTATION
    USES
        Classes;

    CONST
        tolerance = 1.0E-6;

    FUNCTION diagonal_dominance(coefficient_matrix: matrix) : diagonal_dominance_result;

    VAR
        row, column: integer;
        sum_of_first_half, sum_of_second_half, sum: real;
        answer: diagonal_dominance_result;

    BEGIN
        answer.test := true;
        answer.equation_number := 1;

        row := 1;
        WHILE (answer.test = true) AND (row <= number_of_variables) DO
        BEGIN
              sum_of_first_half := 0.0;
              FOR column := 1 TO row - 1 DO
              BEGIN
                    sum_of_first_half := sum_of_first_half + abs(coefficient_matrix[row, column])
              END;

              sum_of_second_half := 0.0;
              FOR column := row + 1 TO number_of_variables DO
              BEGIN
                    sum_of_second_half := sum_of_second_half + abs(coefficient_matrix[row, column])
              END;

              sum := sum_of_first_half + sum_of_second_half;

              answer.test := answer.test AND (abs(coefficient_matrix[row, row]) > sum);
              row := row + 1
          END;

        answer.equation_number := row;
        diagonal_dominance := answer
    END;

    FUNCTION calculate(coefficient_matrix : matrix; constants : standard_array) : standard_array;

    VAR
        index, column, row : integer;
        accurate : boolean;

    BEGIN
        FOR index := 1 TO number_of_variables DO
        BEGIN
            x[index] := random
        END;

        accurate := false;
        WHILE NOT accurate DO
        BEGIN
            FOR column := 1 TO index - 1 DO
            BEGIN
                sum_of_first_half := sum_of_first_half + coefficient_matrix[row, column] * x[column];
            END;

            FOR index := 1 TO number_of_variables DO
            BEGIN
                x_new[index] := (b[index] - sum_of_first_half - sum_of_second_half) / coefficient_matrix[index, index]
            END
        END
    END;
END.

