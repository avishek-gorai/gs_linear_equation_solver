! This program solves linear equation using Gauss-Seidel iteration method.
! Copyright (C) 2025 Avishek Gorai <avishekgorai@myyahoo.com>

! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.

! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https://www.gnu.org/licenses/>.

PROGRAM gauss_seidel_linear_equation_solver
  IMPLICIT NONE

  REAL, ALLOCATABLE, DIMENSION(:) :: x, b, x_new
  REAL, ALLOCATABLE, DIMENSION(:, :) :: a
  REAL, PARAMETER :: epsilon = 1.0E-6

  INTEGER number_of_variables, array_allocation_status, i, j

  REAL sum_of_first_half, sum_of_second_half

  LOGICAL accurate, diagonal_domincance_condtition

  PRINT *, "Enter number of variables: "
  READ *, number_of_variables

  ALLOCATE (x(number_of_variables), a(number_of_variables, number_of_variables), b(number_of_variables)&
       , x_new(number_of_variables), stat = array_allocation_status)

  IF (array_allocation_status == 0) THEN
     PRINT *, "Enter the values of co-efficients:-"
     READ *, a

     CALL diagonal_domincance(diagonal_domincance_condtition)
     
     IF (diagonal_domincance_condtition .EQV. .TRUE.) THEN
        PRINT *, "Enter the values of constants:-"
        READ *, b

        PRINT *, "Enter guessed values:-"
        READ *, x

        DO
           DO i = 1, number_of_variables
              sum_of_first_half = 0
              DO j = 1, i - 1
                 sum_of_first_half = sum_of_first_half + a(i, j)
              END DO

              sum_of_second_half = 0
              DO j = i + 1, number_of_variables
                 sum_of_second_half = sum_of_second_half + a(i, j)
              END DO

              x_new(i) = (b(i) - sum_of_first_half - sum_of_second_half) / a(i, i)
           END DO

           accurate = .TRUE.
           DO i = 1, number_of_variables
              accurate = accurate .AND. abs(x_new(i) - x(i)) < epsilon
           END DO
           
           IF (accurate) EXIT
        END DO

        PRINT *, x_new
     ELSE
        PRINT *, "Diagonal dominance unsatisfied!"        
     END IF
     DEALLOCATE (x(number_of_variables), a(number_of_variables, number_of_variables), b(number_of_variables),&
          x_new(number_of_variables), stat = array_allocation_status)
     IF (array_allocation_status /= 0) PRINT *, "Could not deallocate arrays!"
  ELSE
     PRINT *, "Could not allocate all the arrays!"
  END IF

CONTAINS
  SUBROUTINE diagonal_domincance(condition)
    LOGICAL, INTENT(OUT) :: condition
    
    REAL sum_of_first_half, sum_of_second_half, sum_of_other_coefficients
    INTEGER i, j

    condition = .TRUE.
    DO i = 1, number_of_variables
       sum_of_first_half = 0
       DO j = 1, i-1
          sum_of_first_half = sum_of_first_half + abs(a(i, j))
       END DO

       sum_of_second_half = 0
       DO j = i+1, number_of_variables
          sum_of_second_half = sum_of_second_half + abs(a(i, j))
       END DO

       sum_of_other_coefficients = sum_of_first_half + sum_of_second_half

       condition = condition .AND. abs(a(i, i)) > sum_of_other_coefficients
    END DO
  END SUBROUTINE diagonal_domincance

END PROGRAM gauss_seidel_linear_equation_solver
