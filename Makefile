src_path=src/

.PHONY: all

all: base multicore
#multicore base gpu


#base:
#	gfortran -ffree-line-length-none -g -o test-parallelexample -O2   ${src_path}functions.f90 ${src_path}potentialnp.f90

multicore:
	pgfortran -o test-parallelexample-multi -acc -ta=multicore -fast -Minfo=all ${src_path}functions.f90 ${src_path}potentialnp.f90

base:
	pgfortran -o test-parallelexample -O2  -fast -Minfo=all ${src_path}functions.f90 ${src_path}potentialnp.f90

gpu:
	pgfortran -o test-parallelexample-gpu -acc -ta=tesla:managed -fast -Minfo=all ${src_path}functions.f90 ${src_path}potentialnp.f90

clean:
	rm *.o
	rm *.mod
