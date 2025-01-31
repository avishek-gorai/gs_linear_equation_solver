GS_solver.exe: GS_solver.f90
	gfortran -g -pg --std=f95 GS_solver.f90 -o GS_solver.exe
