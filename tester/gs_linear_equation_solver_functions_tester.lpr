PROGRAM gs_linear_equation_solver_functions_tester;

USES
    Classes,
    ConsoleTestRunner,
    TestCase1;

TYPE
    TMyTestRunner = CLASS(TTestRunner)
    PROTECTED
    // override the protected methods of TTestRunner to customize its behavior
    END;

VAR
    tester : TMyTestRunner;

BEGIN
    tester := TMyTestRunner.create(NIL);
    tester.initialize;
    tester.title := 'Gauss-Seidel Linear Equation Solver Functions Tester';
    tester.run;
    tester.free
END.
