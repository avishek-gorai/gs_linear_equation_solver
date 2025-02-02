! GS_solver.f90 -- This program solves linear equation using Gauss-Seidel iteration method.
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

PROGRAM gauss_seidel_solver
  IMPLICIT NONE

  REAL, PARAMETER :: tolerance = epsilon(1.0) * 1.0E2

  REAL, ALLOCATABLE, DIMENSION(:)    :: x, b, x_new
  REAL, ALLOCATABLE, DIMENSION(:, :) :: coefficient_matrix
  INTEGER number_of_variables, array_allocation_status, i, j, array_deallocation_status
  REAL sum_of_first_half, sum_of_second_half, n
  LOGICAL accurate, diagonal_dominance


  PRINT *, "Enter number of variables: "
  READ *, number_of_variables
  PRINT *

  ALLOCATE (x(number_of_variables), coefficient_matrix(number_of_variables, number_of_variables),&
       b(number_of_variables), x_new(number_of_variables), stat = array_allocation_status)

  IF (array_allocation_status /= 0) THEN
     PRINT *, "Could not allocate all the arrays! Code: ", array_allocation_status
  ELSE
     PRINT *, "Enter the coefficients in column major order:-"
     READ *, coefficient_matrix
     PRINT *

     diagonal_dominance = .TRUE.
     DO i = 1, number_of_variables
        sum_of_first_half = 0.0
        DO j = 1, i - 1
           sum_of_first_half = sum_of_first_half + coefficient_matrix(i, j)
        END DO

        sum_of_second_half = 0.0
        DO j = i+1, number_of_variables
           sum_of_second_half = sum_of_second_half + coefficient_matrix(i, j)
        END DO

        diagonal_dominance = diagonal_dominance .AND. (abs(coefficient_matrix(i, i)) > (sum_of_first_half + sum_of_second_half))

        IF (.NOT. diagonal_dominance)  EXIT
     END DO

     IF (.NOT. diagonal_dominance) THEN
        PRINT *, "Diagonal dominance not satisfied by equation number ", i
     ELSE
        PRINT *, "Enter the values of constants:-"
        READ *, b
        PRINT *

        DO i = 1, number_of_variables
           CALL random_number(n)
           x(i) = n
        END DO

        PRINT *, "Choosen random values of x:-"
        PRINT *, x
        PRINT *

        DO
           DO i = 1, number_of_variables
              sum_of_first_half = 0.0
              DO j = 1, i - 1
                 sum_of_first_half = sum_of_first_half + x_new(j) * coefficient_matrix(i, j)
              END DO

              sum_of_second_half = 0.0
              DO j = i + 1, number_of_variables
                 sum_of_second_half = sum_of_second_half + x(j) * coefficient_matrix(i, j)
              END DO

              x_new(i) = (b(i) - sum_of_first_half - sum_of_second_half) / coefficient_matrix(i, i)
           END DO

           accurate = .TRUE.
           DO i = 1, number_of_variables
              accurate = accurate .AND. (abs(x_new(i) - x(i)) < tolerance)
           END DO

           PRINT *, x_new

           IF (accurate)  EXIT
        END DO

        PRINT *, x_new
     END IF
     DEALLOCATE (x, coefficient_matrix, b, x_new, stat = array_deallocation_status)
     IF (array_deallocation_status /= 0) PRINT *, "Could not deallocate arrays! Code: ", array_deallocation_status
  END IF
END PROGRAM gauss_seidel_solver
