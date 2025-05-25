UNIT TestCase1;

INTERFACE
    USES
        Classes,
        SysUtils,
        fpcunit,
        testutils,
        testregistry;

    TYPE
        TTestCase1 = CLASS(TTestCase)
        PROTECTED
            PROCEDURE setUp;
            OVERRIDE;

            PROCEDURE tearDown;
            OVERRIDE;
        PUBLISHED
            PROCEDURE testHookUp;
        END;

IMPLEMENTATION
    PROCEDURE TTestCase1.testHookUp;

    BEGIN
      Fail('Write your own test');
    END;

    PROCEDURE TTestCase1.setUp;

    BEGIN

    END;

    PROCEDURE TTestCase1.tearDown;

    BEGIN

    END;

INITIALIZATION
    RegisterTest(TTestCase1);
END.

