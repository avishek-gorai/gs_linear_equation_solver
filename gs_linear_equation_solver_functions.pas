UNIT gs_linear_equation_solver_functions;

INTERFACE
    CONST
        variable_limit = 30;
        tolerance      = 1.0E-6;

    TYPE
        maximum_variables         = 1 .. variable_limit;
        matrix                    = ARRAY [1 .. variable_limit, 1 .. variable_limit] OF real;
        standard_array            = ARRAY [1 .. variable_limit] OF real;
        diagonal_dominance_result = RECORD
            equation_number : maximum_variables;
            test            : boolean;
        END;

    FUNCTION calculate(coefficient_matrix  : matrix;
                       constants           : standard_array;
                       number_of_variables : maximum_variables) : standard_array;

    PROCEDURE read_coefficients(VAR coefficient_matrix  : matrix;
                                    number_of_variables : maximum_variables);

    PROCEDURE read_constants(VAR constants           : standard_array;
                                 number_of_variables : maximum_variables);

    FUNCTION diagonal_dominance(coefficient_matrix  : matrix;
                                number_of_variables : maximum_variables) : diagonal_dominance_result;

IMPLEMENTATION
    USES
        Sysutils;

    PROCEDURE read_constants(VAR constants           : standard_array;
                                 number_of_variables : maximum_variables);

    VAR
        index : maximum_variables;

    BEGIN
        FOR index := 1 TO number_of_variables DO
        BEGIN
            read(constants[index]);
        END
    END;

    PROCEDURE read_coefficients(VAR coefficient_matrix  : matrix;
                                    number_of_variables : maximum_variables);

    VAR
        row, column : maximum_variables;

    BEGIN
        FOR row := 1 TO number_of_variables DO
        BEGIN
            FOR column := 1 TO number_of_variables DO
            BEGIN
                read(coefficient_matrix[row][column])
            END
        END
    END;

    FUNCTION diagonal_dominance(coefficient_matrix  : matrix;
                                number_of_variables : maximum_variables) : diagonal_dominance_result;

    VAR
        row, column                          : maximum_variables;
        first_half_sum, second_half_sum, sum : real;
        answer                               : diagonal_dominance_result;

    BEGIN
        answer.test := true;
        answer.equation_number := 1;

        row := 1;
        WHILE (answer.test = true) AND (row <= number_of_variables) DO
        BEGIN
            first_half_sum := 0.0;
            FOR column := 1 TO row - 1 DO
            BEGIN
                first_half_sum := first_half_sum + abs(coefficient_matrix[row, column])
            END;

            second_half_sum := 0.0;
            FOR column := row + 1 TO number_of_variables DO
            BEGIN
                second_half_sum := second_half_sum + abs(coefficient_matrix[row, column])
            END;

            sum := first_half_sum + second_half_sum;

            answer.test := answer.test AND (abs(coefficient_matrix[row, row]) > sum);
            row := row + 1
        END;

        IF answer.test = false THEN
          BEGIN
            answer.equation_number := row;
          END;
        diagonal_dominance := answer
    END;

    FUNCTION calculate(coefficient_matrix  : matrix;
                       constants           : standard_array;
                       number_of_variables : maximum_variables) : standard_array;

    VAR
        index, column, row              : maximum_variables;
        accurate                        : boolean;
        x, x_new                        : standard_array;
        first_half_sum, second_half_sum : real;
        dd_result                       : diagonal_dominance_result;

    BEGIN
        (* Assigning random values. *)
        FOR index := 1 TO number_of_variables DO
        BEGIN
            x[index] := random
        END;

        accurate := false;
        row := 1;
        REPEAT
            (* Calculating new values. *)
            FOR row := 1 TO number_of_variables DO
            BEGIN
                first_half_sum := 0;
                FOR column := 1 TO row - 1 DO
                BEGIN
                    first_half_sum := first_half_sum + coefficient_matrix[row, column] * x[column];
                END;

                second_half_sum := 0;
                FOR column := row + 1 TO number_of_variables DO
                BEGIN
                    second_half_sum := second_half_sum + coefficient_matrix[row, column] * x[column];
                END;

                x_new[row] := (constants[row] - first_half_sum - second_half_sum) / coefficient_matrix[row, row]
            END;

            (* Checking accuracy. *)
            index := 1;
            accurate := true;
            WHILE index <= number_of_variables DO
            BEGIN
                accurate := accurate AND (abs(x_new[index] - x[index]) < tolerance);
                index := index + 1
            END;

            IF NOT accurate THEN
            BEGIN
                (* Writing values *)
                FOR index := 1 TO number_of_variables DO
                BEGIN
                    x[index] := x_new[index]
                END
            END
        UNTIL accurate;

        calculate := x_new
    END;
END.

