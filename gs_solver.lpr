PROGRAM gauss_seidel_solver;

CONST
    maximum_variables = 30;
    blank = ' ';

TYPE
    matrix = ARRAY [1 .. maximum_variables, 1 .. maximum_variables] OF real;
    natural_number = 1 .. maxint;

VAR
    serial_number, equation_number : natural_number;
    number_of_variables : 1 .. maximum_variables;
    coefficient_matrix : matrix;
    b, x, x_new : ARRAY [1 .. maximum_variables] OF integer;

PROCEDURE read_coefficients;

BEGIN
END;

PROCEDURE read_b;

BEGIN
END;

FUNCTION diagonal_dominance(var equation_number : natural_number) : boolean;

BEGIN
    diagonal_dominance := true;
    equation_number := 1
END;

PROCEDURE calculate;

BEGIN
END;

PROCEDURE print_results;

VAR
    index : integer;

BEGIN
    FOR index := 1 TO number_of_variables DO
    BEGIN
        write(x_new[index], blank)
    END;
    writeln
END;

BEGIN
    serial_number := 1;
    WHILE NOT eof DO
    BEGIN
        write(serial_number, blank);
        read(number_of_variables);
        IF eoln THEN
        BEGIN
            readln
        END;

        IF NOT eof THEN
        BEGIN
            read_coefficients;
            read_b;

            IF diagonal_dominance(equation_number) THEN
            BEGIN
                calculate;
                print_results
            END
        ELSE
        BEGIN
            writeln('Diagonal dominance not satisfied by equation number: ', equation_number)
        END
        END;
        serial_number := serial_number + 1
    END;
    writeln('Done.')
END.
