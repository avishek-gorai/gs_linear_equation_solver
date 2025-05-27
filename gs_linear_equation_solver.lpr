PROGRAM gs_linear_equation_solver;

USES
    gs_linear_equation_solver_functions,
    Classes,
    SysUtils,
    CustApp;

TYPE
    GSLinearEquationSolver = CLASS(TCustomapplication)
    PROTECTED
        PROCEDURE doRun;
        OVERRIDE;

    PUBLIC
        CONSTRUCTOR create(owner : TComponent);
        OVERRIDE;

        DESTRUCTOR destroy;
        OVERRIDE;

        FUNCTION writeHelp : GSLinearEquationSolver;
        VIRTUAL;
    END;

    PROCEDURE GSLinearEquationSolver.doRun;

    VAR
        error_message : string;

    BEGIN
        error_message := self.checkOptions('h', 'help');
        IF error_message <> '' THEN
        BEGIN
            self.showException(Exception.create(error_message));
            self.terminate
        END;

        IF self.hasOption('h', 'help') THEN
        BEGIN
            self.writeHelp;
            self.terminate
        END;

        self.terminate
    END;

    CONSTRUCTOR GSLinearEquationSolver.create(the_owner : TComponent);

    BEGIN
      INHERITED create(the_owner);
      self.stopOnException := true
    END;

    DESTRUCTOR GSLinearEquationSolver.destroy;

    BEGIN
      INHERITED destroy;
    END;

    FUNCTION GSLinearEquationSolver.writeHelp : GSLinearEquationSolver;

    BEGIN
        writeln('Usage: ', self.exeName, ' -h');
        writeHelp := self
    END;

VAR
    application : GSLinearEquationSolver;

BEGIN
    application := GSLinearEquationSolver.create(NIL);
    application.title := 'Gauss-Seidel Linear Equation Solver';
    application.run;
    application.free
END.

